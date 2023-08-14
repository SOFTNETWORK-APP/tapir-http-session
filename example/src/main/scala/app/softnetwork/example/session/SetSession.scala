package app.softnetwork.example.session

import app.softnetwork.session.CsrfEndpoints._
import app.softnetwork.session.SessionEndpoints._
import app.softnetwork.session.TapirCsrfOptions._
import app.softnetwork.session.TapirEndpoints
import app.softnetwork.session.TapirSessionOptions._
import com.softwaremill.example.session.MyScalaSession
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir.model.UsernamePassword
import sttp.tapir.server.ServerEndpoint
import sttp.tapir._

import scala.concurrent.Future

object SetSessionApp extends SetSession

trait SetSession extends TapirApp with TapirEndpoints {

  import system.dispatcher

  protected val login: ServerEndpoint[Any, Future] =
    setNewCsrfToken(checkHeader) { // generate a new CSRF token
      setSession(refreshable, usingCookies) { // set the session using refreshable session continuity and cookies
        endpoint
          .securityIn(auth.basic[UsernamePassword](WWWAuthenticateChallenge.basic("example")))
          .serverSecurityLogicSuccessWithOutput(credentials =>
            Future.successful(
              ((), Some(MyScalaSession(credentials.username)))
            ) // the session value is set using basic authentication
          )
      }
    }.post
      .in("login")
      .out(stringBody)
      .serverLogicSuccess(maybeSession =>
        _ => Future.successful("Hello " + maybeSession.map(_.username).getOrElse(""))
      )

  override val endpoints: List[ServerEndpoint[Any, Future]] =
    List(
      login
    )

}
