package app.softnetwork.session

import com.softwaremill.session._
import sttp.tapir._
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput

import scala.concurrent.{ExecutionContext, Future}

trait SessionEndpoints {

  /** Set the session cookie with the session content. The content is signed, optionally encrypted
    * and with an optional expiry date.
    *
    * If refreshable, generates a new token (removing old ones) and stores it in the refresh token
    * cookie.
    */
  def setSession[T, SECURITY_INPUT, SECURITY_OUTPUT, ERROR_OUTPUT](
    sc: TapirSessionContinuity[T],
    st: SetSessionTransport
  )(
    body: => PartialServerEndpointWithSecurityOutput[SECURITY_INPUT, Option[
      T
    ], Unit, ERROR_OUTPUT, SECURITY_OUTPUT, Unit, Any, Future]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(SECURITY_INPUT, Seq[Option[String]]), Option[
    T
  ], Unit, ERROR_OUTPUT, (SECURITY_OUTPUT, Seq[Option[String]]), Unit, Any, Future] =
    sc.setSession(st)(body)

  def setSessionWithAuth[T, A](sc: TapirSessionContinuity[T], st: SetSessionTransport)(
    auth: => EndpointInput.Auth[A, EndpointInput.AuthType.Http]
  )(implicit
    f: A => Option[T],
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[
    (A, Seq[Option[String]]),
    Option[T],
    Unit,
    Unit,
    (
      Unit,
      Seq[
        Option[String]
      ]
    ),
    Unit,
    Any,
    Future
  ] =
    setSession[T, A, Unit, Unit](sc, st) {
      endpoint
        .securityIn(auth)
        .serverSecurityLogicSuccessWithOutput(credentials =>
          Future.successful(
            ((), f(credentials))
          )
        )
    }

  /** Read a session from the session cookie, wrapped in [[SessionResult]] describing the possible
    * success/failure outcomes.
    *
    * If refreshable, tries to create a new session based on the refresh token cookie.
    */
  def session[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] =
    sc.session(gt, required)

  /** Invalidate the session cookie.
    *
    * If refreshable, also removes the refresh token cookie and the refresh token token (from the
    * client and token store), if present.
    *
    * Note that you should use `refreshable` if you use refreshable systems even only for some
    * users.
    */
  def invalidateSession[T, SECURITY_INPUT, PRINCIPAL, ERROR_OUTPUT](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport
  )(
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
    sc.invalidateSession(gt)(body)

  /** Read an optional session from the session cookie.
    */
  def optionalSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], Option[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] =
    sc.optionalSession(gt)

  /** Read a required session from the session cookie.
    */
  def requiredSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], T, Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] =
    sc.requiredSession(gt)

  def touchSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport,
    required: Option[Boolean]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], SessionResult[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] =
    sc.touchSession(gt, required)

  /** Sets the session cookie again with the same data. Useful when using the
    * [[SessionConfig.sessionMaxAgeSeconds]] option, as it sets the expiry date anew.
    */
  def touchOptionalSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], Option[T], Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    sc.touchOptionalSession(gt)
  }

  /** Sets the session cookie again with the same data. Useful when using the
    * [[SessionConfig.sessionMaxAgeSeconds]] option, as it sets the expiry date anew.
    */
  def touchRequiredSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[Seq[Option[String]], T, Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    sc.touchRequiredSession(gt)
  }
}

object TapirSessionOptions {
  def oneOff[T](implicit manager: SessionManager[T], ec: ExecutionContext) = new OneOffTapir[T]()
  def refreshable[T](implicit
    manager: SessionManager[T],
    refreshTokenStorage: RefreshTokenStorage[T],
    ec: ExecutionContext
  ) = new RefreshableTapir[T]()
  def usingCookies: SetSessionTransport = CookieST
  def usingHeaders: SetSessionTransport = HeaderST
  def usingCookiesOrHeaders: GetSessionTransport = CookieOrHeaderST
}

class OneOffTapir[T](implicit val manager: SessionManager[T], val ec: ExecutionContext)
    extends OneOffTapirSessionContinuity[T]
    with OneOffTapirSession[T]

class RefreshableTapir[T](implicit
  val manager: SessionManager[T],
  val refreshTokenStorage: RefreshTokenStorage[T],
  val ec: ExecutionContext
) extends RefreshableTapirSessionContinuity[T]
    with OneOffTapirSession[T]
    with RefreshableTapirSession[T]

object SessionEndpoints extends SessionEndpoints
