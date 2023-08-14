package app.softnetwork.example.session

import app.softnetwork.session.TapirCsrfOptions.checkHeader
import app.softnetwork.session.TapirSessionOptions._
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

object CsrfApp extends Csrf

trait Csrf extends SetSession {

  import system.dispatcher

  private[this] val sensitive =
    hmacTokenCsrfProtection(checkHeader) { // check CSRF token
      requiredSession(refreshable, usingCookies) // a session is required
    }
      .description(
        """Sensitive endpoint.
          |First you will have to log in.
          |Then you will have to provide the CSRF token as an header (X-XSRF-TOKEN header),
          |using the value of the XSRF-TOKEN cookie.""".stripMargin
      )
      .post
      .in("sensitive")
      .in(stringBody.description("the sensitive information to be protected"))
      .out(stringBody)
      .serverLogicSuccess(session =>
        body => Future.successful("Hello " + session.username + ", you sent: " + body)
      )

  override val endpoints: List[ServerEndpoint[Any, Future]] =
    List(
      login,
      sensitive
    )
}
