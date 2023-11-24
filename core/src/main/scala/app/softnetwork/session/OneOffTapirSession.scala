package app.softnetwork.session

import akka.http.scaladsl.model.DateTime
import com.softwaremill.session._
import sttp.model.StatusCode
import sttp.model.headers.CookieValueWithMeta
import sttp.monad.FutureMonad
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput
import sttp.tapir.{cookie, header, _}

import scala.concurrent.{ExecutionContext, Future}

private[session] trait OneOffTapirSession[T] {

  implicit def ec: ExecutionContext

  private[this] def getSessionFromClientAsCookie(implicit
    manager: SessionManager[T]
  ): EndpointInput.Cookie[Option[String]] = {
    cookie(manager.config.sessionCookieConfig.name)
  }

  private[this] def sendSessionToClientAsCookie(implicit
    manager: SessionManager[T]
  ): EndpointIO.Header[Option[CookieValueWithMeta]] = {
    setCookieOpt(manager.config.sessionCookieConfig.name)
  }

  private[this] def getSessionFromClientAsHeader(implicit
    manager: SessionManager[T]
  ): EndpointIO.Header[Option[String]] = {
    header[Option[String]](manager.config.sessionHeaderConfig.getFromClientHeaderName)
  }

  private[this] def sendSessionToClientAsHeader(implicit
    manager: SessionManager[T]
  ): EndpointIO.Header[Option[String]] = {
    header[Option[String]](manager.config.sessionHeaderConfig.sendToClientHeaderName)
  }

  def setSession[SECURITY_INPUT, ERROR_OUTPUT, SECURITY_OUTPUT](st: SetSessionTransport)(
    body: => PartialServerEndpointWithSecurityOutput[
      SECURITY_INPUT,
      Option[T],
      Unit,
      ERROR_OUTPUT,
      SECURITY_OUTPUT,
      Unit,
      Any,
      Future
    ]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(SECURITY_INPUT, Seq[Option[String]]), Option[
    T
  ], Unit, ERROR_OUTPUT, (SECURITY_OUTPUT, Seq[Option[String]]), Unit, Any, Future] =
    st match {
      case CookieST => setOneOffCookieSession(body)
      case HeaderST => setOneOffHeaderSession(body)
    }

  def setSessionLogic(session: Option[T], existing: Option[String])(implicit
    manager: SessionManager[T]
  ): Option[String] = setOneOffSessionLogic(session, existing)

  protected def setOneOffSessionLogic(session: Option[T], existing: Option[String])(implicit
    manager: SessionManager[T]
  ): Option[String] =
    session.map(manager.clientSessionManager.encode).fold(existing)(Some(_))

  protected def setOneOffCookieSession[SECURITY_INPUT, ERROR_OUTPUT, SECURITY_OUTPUT](
    body: => PartialServerEndpointWithSecurityOutput[
      SECURITY_INPUT,
      Option[T],
      Unit,
      ERROR_OUTPUT,
      SECURITY_OUTPUT,
      Unit,
      Any,
      Future
    ]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(SECURITY_INPUT, Seq[Option[String]]), Option[
    T
  ], Unit, ERROR_OUTPUT, (SECURITY_OUTPUT, Seq[Option[String]]), Unit, Any, Future] =
    body.endpoint
      .securityIn(getSessionFromClientAsCookie.map(Seq(_))(_.head))
      .out(body.securityOutput)
      .out(
        sendSessionToClientAsCookie.map(o => Seq(o.map(_.value)))(
          _.head.map(manager.clientSessionManager.createCookieWithValue(_).valueWithMeta)
        )
      )
      .serverSecurityLogicWithOutput { inputs =>
        body.securityLogic(new FutureMonad())(inputs._1) map {
          case Left(l) => Left(l)
          case Right(r) =>
            val session = r._2
            val existing = inputs._2.head
            Right(setOneOffSessionLogic(session, existing)).map(result =>
              (
                (r._1, Seq(result)),
                session
              )
            )
        }
      }

  protected def setOneOffHeaderSession[SECURITY_INPUT, ERROR_OUTPUT, SECURITY_OUTPUT](
    body: => PartialServerEndpointWithSecurityOutput[
      SECURITY_INPUT,
      Option[T],
      Unit,
      ERROR_OUTPUT,
      SECURITY_OUTPUT,
      Unit,
      Any,
      Future
    ]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(SECURITY_INPUT, Seq[Option[String]]), Option[
    T
  ], Unit, ERROR_OUTPUT, (SECURITY_OUTPUT, Seq[Option[String]]), Unit, Any, Future] =
    body.endpoint
      .securityIn(getSessionFromClientAsHeader.map(Seq(_))(_.head))
      .out(body.securityOutput)
      .out(sendSessionToClientAsHeader.map(Seq(_))(_.head))
      .serverSecurityLogicWithOutput { inputs =>
        body.securityLogic(new FutureMonad())(inputs._1) map {
          case Left(l) => Left(l)
          case Right(r) =>
            val session = r._2
            val existing = inputs._2.head
            Right(setOneOffSessionLogic(session, existing)).map(result =>
              (
                (r._1, Seq(result)),
                r._2
              )
            )
        }
      }

  def session(
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = oneOffSession(gt, required)

  protected def oneOffSession(
    gt: GetSessionTransport,
    required: Option[Boolean] = None
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] =
    gt match {
      case CookieST         => oneOffCookieSession(required)
      case HeaderST         => oneOffHeaderSession(required)
      case CookieOrHeaderST => oneOffCookieOrHeaderSession(required)
    }

  protected def oneOffCookieSession(
    required: Option[Boolean] = None
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[
    T
  ], Unit, Unit, Seq[Option[String]], Unit, Any, Future] = {
    endpoint
      .securityIn(getSessionFromClientAsCookie)
      .mapSecurityIn(Seq(_))(_.head)
      .out(
        sendSessionToClientAsCookie.map(o => Seq(o.map(_.value)))(oo =>
          oo.head.map(manager.clientSessionManager.createCookieWithValue(_).valueWithMeta)
        )
      )
      .errorOut(statusCode(StatusCode.Unauthorized))
      .serverSecurityLogicWithOutput { inputs =>
        Future.successful(
          oneOffSessionLogic(None, inputs.head, None, CookieST, required)
            .map(e => (e._1.head, e._2)) match {
            case Left(l)  => Left(l)
            case Right(r) => Right((Seq(r._1), r._2))
          }
        )
      }
  }

  def extractSession(maybeValue: Option[String])(implicit manager: SessionManager[T]): Option[T] =
    extractOneOffSession(maybeValue)

  protected def extractOneOffSession(maybeValue: Option[String])(implicit
    manager: SessionManager[T]
  ): Option[T] =
    maybeValue.flatMap(manager.clientSessionManager.decode(_).toOption)

  protected def oneOffHeaderSession(
    required: Option[Boolean] = None
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[
    T
  ], Unit, Unit, Seq[Option[String]], Unit, Any, Future] =
    endpoint
      .securityIn(
        getSessionFromClientAsHeader
      )
      .mapSecurityIn(Seq(_))(_.head)
      .out(sendSessionToClientAsHeader.map(Seq(_))(_.head))
      .errorOut(statusCode(StatusCode.Unauthorized))
      .serverSecurityLogicWithOutput { inputs =>
        Future.successful(
          oneOffSessionLogic(None, None, inputs.head, HeaderST, required)
            .map(e => (e._1.head, e._2)) match {
            case Left(l)  => Left(l)
            case Right(r) => Right((Seq(r._1), r._2))
          }
        )
      }

  def sessionLogic(
    existingSession: Option[SessionResult[T]],
    maybeCookie: Option[String],
    maybeHeader: Option[String],
    gt: GetSessionTransport,
    required: Option[Boolean],
    touch: Boolean = false
  )(implicit manager: SessionManager[T]): Either[Unit, (Seq[Option[String]], SessionResult[T])] =
    oneOffSessionLogic(existingSession, maybeCookie, maybeHeader, gt, required, touch)

  private[this] def oneOffSessionLogic(
    existingSession: Option[SessionResult[T]],
    maybeCookie: Option[String],
    maybeHeader: Option[String],
    gt: GetSessionTransport,
    required: Option[Boolean],
    touch: Boolean = false
  )(implicit manager: SessionManager[T]): Either[Unit, (Seq[Option[String]], SessionResult[T])] = {
    // read session from the cookie and/or header
    val session = maybeCookie.fold(maybeHeader)(Some(_))

    def touchSession(): Either[Unit, (Option[String], SessionResult[T])] = {
      session match {
        case Some(value) =>
          val decoded = manager.clientSessionManager.decode(value)
          decoded match {
            case s: SessionResult.DecodedLegacy[T] =>
              Right(
                (
                  Some(manager.clientSessionManager.encode(s.session)),
                  s
                )
              )
            case s =>
              if (touch) {
                val newSession = s.toOption.map(manager.clientSessionManager.encode)
                Right((newSession, s))
              } else {
                Right(None, s)
              }
          }
        case _ =>
          if (required.getOrElse(false))
            Left(())
          else
            Right((None, SessionResult.NoSession))
      }
    }

    (existingSession match {
      case Some(result) =>
        result match {
          case SessionResult.NoSession | SessionResult.Expired => touchSession()
          case s =>
            if (touch) {
              val newSession = s.toOption.map(manager.clientSessionManager.encode)
              Right((newSession, s))
            } else {
              Right(None, s)
            }
        }
      case _ => touchSession()
    }) match {
      case Left(l) => Left(l)
      case Right((newSession, s)) =>
        gt match {
          case CookieST | HeaderST =>
            Right((Seq(newSession), s))
          case CookieOrHeaderST =>
            Right(
              (
                Seq(
                  maybeCookie.flatMap(_ => newSession),
                  maybeHeader.flatMap(_ => newSession)
                ),
                s
              )
            )
        }
    }
  }

  protected def oneOffCookieOrHeaderSession(
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[
    T
  ], Unit, Unit, Seq[Option[String]], Unit, Any, Future] =
    endpoint
      .securityIn(getSessionFromClientAsCookie)
      .securityIn(getSessionFromClientAsHeader)
      .mapSecurityIn(inputs => Seq(inputs._1, inputs._2))(oo => (oo.head, oo.last))
      .out(
        sendSessionToClientAsCookie
          .and(sendSessionToClientAsHeader)
          .map(outputs => Seq(outputs._1.map(_.value), outputs._2))(oo =>
            (
              oo.head.map(manager.clientSessionManager.createCookieWithValue(_).valueWithMeta),
              oo.last
            )
          )
      )
      .errorOut(statusCode(StatusCode.Unauthorized))
      .serverSecurityLogicWithOutput { inputs =>
        Future.successful(
          oneOffSessionLogic(
            None,
            inputs.head,
            inputs.last,
            CookieOrHeaderST,
            required
          )
            .map(result => (result._1, result._2))
        )
      }

  private[this] def invalidateOneOffSessionLogic[SECURITY_OUTPUT, PRINCIPAL, ERROR_OUTPUT](
    result: (SECURITY_OUTPUT, PRINCIPAL),
    maybeCookie: Option[String],
    maybeHeader: Option[String]
  ): Either[ERROR_OUTPUT, (Seq[Option[String]], PRINCIPAL)] = {
    val principal = result._2
    maybeCookie match {
      case Some(_) =>
        maybeHeader match {
          case Some(_) =>
            Right(
              (
                Seq(
                  Some("deleted"),
                  Some("")
                ),
                principal
              )
            )
          case _ =>
            Right(
              (
                Seq(
                  Some("deleted"),
                  None
                ),
                principal
              )
            )
        }
      case _ =>
        maybeHeader match {
          case Some(_) => Right((Seq(None, Some("")), principal))
          case _       => Right((Seq(None, None), principal))
        }
    }
  }

  def invalidateSession[
    SECURITY_INPUT,
    PRINCIPAL,
    ERROR_OUTPUT
  ](gt: GetSessionTransport)(
    body: => PartialServerEndpointWithSecurityOutput[
      SECURITY_INPUT,
      PRINCIPAL,
      Unit,
      ERROR_OUTPUT,
      _,
      Unit,
      Any,
      Future
    ]
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[
    (SECURITY_INPUT, Seq[Option[String]]),
    PRINCIPAL,
    Unit,
    ERROR_OUTPUT,
    Seq[Option[String]],
    Unit,
    Any,
    Future
  ] = invalidateOneOffSession(gt) {
    body
  }

  protected def invalidateOneOffSession[
    SECURITY_INPUT,
    PRINCIPAL,
    ERROR_OUTPUT
  ](gt: GetSessionTransport)(
    body: => PartialServerEndpointWithSecurityOutput[
      SECURITY_INPUT,
      PRINCIPAL,
      Unit,
      ERROR_OUTPUT,
      _,
      Unit,
      Any,
      Future
    ]
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[
    (SECURITY_INPUT, Seq[Option[String]]),
    PRINCIPAL,
    Unit,
    ERROR_OUTPUT,
    Seq[Option[String]],
    Unit,
    Any,
    Future
  ] =
    body.endpoint
      .securityIn(getSessionFromClientAsCookie)
      .securityIn(getSessionFromClientAsHeader)
      .mapSecurityIn(inputs => (inputs._1, Seq(inputs._2, inputs._3)))(oo =>
        (oo._1, oo._2.head, oo._2.last)
      )
      .out(
        sendSessionToClientAsCookie
          .and(sendSessionToClientAsHeader)
          .map(outputs => Seq(outputs._1.map(_.value), outputs._2))(oo =>
            (
              oo.head.map(
                manager.clientSessionManager
                  .createCookieWithValue(_)
                  .withExpires(DateTime.MinValue)
                  .valueWithMeta
              ),
              oo.last
            )
          )
      )
      .serverSecurityLogicWithOutput { inputs =>
        body.securityLogic(new FutureMonad())(inputs._1).map {
          case Left(l)  => Left(l)
          case Right(r) => invalidateOneOffSessionLogic(r, inputs._2.head, inputs._2.last)
        }
      }

  def touchSession(
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    val partial = oneOffSession(gt, required)
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l) => Left(l)
          case Right(r) =>
            val session = r._2
            gt match {
              case CookieST =>
                oneOffSessionLogic(
                  Some(session),
                  inputs.head,
                  None,
                  CookieST,
                  required,
                  touch = true
                )
              case HeaderST =>
                oneOffSessionLogic(
                  Some(session),
                  None,
                  inputs.head,
                  HeaderST,
                  required,
                  touch = true
                )
              case CookieOrHeaderST =>
                val maybeCookie = inputs.head
                val maybeHeader = inputs.last
                oneOffSessionLogic(
                  Some(session),
                  maybeCookie,
                  maybeHeader,
                  CookieOrHeaderST,
                  required,
                  touch = true
                )
            }
        }
      }
  }

}
