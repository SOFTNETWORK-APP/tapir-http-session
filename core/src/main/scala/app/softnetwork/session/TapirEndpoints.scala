package app.softnetwork.session

import com.softwaremill.session.{GetSessionTransport, SessionManager, SetSessionTransport}
import sttp.model.Method
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.EndpointInput
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput

import scala.concurrent.Future

trait TapirEndpoints extends SessionEndpoints with CsrfEndpoints {

  def antiCsrfWithRequiredSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport,
    checkMode: TapirCsrfCheckMode[T]
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    T,
    Unit,
    Unit,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    hmacTokenCsrfProtection(checkMode) {
      requiredSession(sc, gt)
    }

  def antiCsrfWithOptionalSession[T](
    sc: TapirSessionContinuity[T],
    gt: GetSessionTransport,
    checkMode: TapirCsrfCheckMode[T]
  )(implicit manager: SessionManager[T]): PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    Option[T],
    Unit,
    Unit,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    hmacTokenCsrfProtection(checkMode) {
      optionalSession(sc, gt)
    }

  def setNewCsrfTokenAndSession[T, SECURITY_INPUT, ERROR_OUTPUT, SECURITY_OUTPUT](
    sc: TapirSessionContinuity[T],
    st: SetSessionTransport,
    checkMode: TapirCsrfCheckMode[T]
  )(
    body: => PartialServerEndpointWithSecurityOutput[SECURITY_INPUT, Option[
      T
    ], Unit, ERROR_OUTPUT, SECURITY_OUTPUT, Unit, Any, Future]
  )(implicit
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(SECURITY_INPUT, Seq[Option[String]]), Option[
    T
  ], Unit, ERROR_OUTPUT, ((SECURITY_OUTPUT, Seq[Option[String]]), Option[CookieValueWithMeta]), Unit, Any, Future] =
    setNewCsrfToken(checkMode) {
      setSession(sc, st) {
        body
      }
    }

  def setNewCsrfTokenAndSessionWithAuth[T, A](
    sc: TapirSessionContinuity[T],
    st: SetSessionTransport,
    checkMode: TapirCsrfCheckMode[T]
  )(auth: => EndpointInput.Auth[A, EndpointInput.AuthType.Http])(implicit
    f: A => Option[T],
    manager: SessionManager[T]
  ): PartialServerEndpointWithSecurityOutput[(A, Seq[Option[String]]), Option[
    T
  ], Unit, Unit, ((Unit, Seq[Option[String]]), Option[CookieValueWithMeta]), Unit, Any, Future] =
    setNewCsrfToken(checkMode) {
      setSessionWithAuth(sc, st) {
        auth
      }
    }
}

object TapirEndpoints extends TapirEndpoints
