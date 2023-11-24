package app.softnetwork.session

import com.softwaremill.session.{
  GetSessionTransport,
  SessionManager,
  SessionResult,
  SetSessionTransport
}
import sttp.monad.FutureMonad
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

sealed trait TapirSessionContinuity[T] {

  implicit def ec: ExecutionContext

  def options2session(
    values: Seq[Option[String]]
  )(implicit manager: SessionManager[T]): Option[T] = //TODO find a more appropriate name
    values.flatMap(extractSession).headOption

  def extractSession(maybeValue: Option[String])(implicit manager: SessionManager[T]): Option[T]

  def sessionLogic(
    existingSession: Option[SessionResult[T]],
    maybeCookie: Option[String],
    maybeHeader: Option[String],
    gt: GetSessionTransport,
    required: Option[Boolean],
    touch: Boolean = false
  )(implicit manager: SessionManager[T]): Either[Unit, (Seq[Option[String]], SessionResult[T])]

  def setSessionLogic(session: Option[T], existing: Option[String])(implicit
    manager: SessionManager[T]
  ): Option[String]

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
  ], Unit, ERROR_OUTPUT, (SECURITY_OUTPUT, Seq[Option[String]]), Unit, Any, Future]

  def session(
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future]

  final def optionalSession(
    gt: GetSessionTransport
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], Option[T], Unit, Unit, Seq[Option[String]], Unit, Any, Future] = {
    val partial = session(gt, Some(false))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l)  => Left(l)
          case Right(r) => Right((r._1, r._2.toOption))
        }
      }
  }

  final def requiredSession(
    gt: GetSessionTransport
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], T, Unit, Unit, Seq[Option[String]], Unit, Any, Future] = {
    val partial = session(gt, Some(true))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l) => Left(l)
          case Right(r) =>
            r._2.toOption match {
              case Some(session) => Right((r._1, session))
              case _             => Left(())
            }
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
  ]

  def touchSession(
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future]

  final def touchOptionalSession(
    gt: GetSessionTransport
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], Option[T], Unit, Unit, Seq[Option[String]], Unit, Any, Future] = {
    val partial = touchSession(gt, Some(false))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l)  => Left(l)
          case Right(r) => Right((r._1, r._2.toOption))
        }
      }
  }

  final def touchRequiredSession(
    gt: GetSessionTransport
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], T, Unit, Unit, Seq[Option[String]], Unit, Any, Future] = {
    val partial = touchSession(gt, Some(true))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l) => Left(l)
          case Right(r) =>
            r._2.toOption match {
              case Some(session) => Right((r._1, session))
              case _             => Left(())
            }
        }
      }
  }

}

trait OneOffTapirSessionContinuity[T] extends TapirSessionContinuity[T] {
  _: OneOffTapirSession[T] =>
}

trait RefreshableTapirSessionContinuity[T] extends TapirSessionContinuity[T] with Completion {
  this: RefreshableTapirSession[T] with OneOffTapirSession[T] =>

  def removeToken(value: String)(implicit manager: SessionManager[T]): Try[Unit] =
    refreshable.refreshTokenManager.removeToken(value).complete()

}
