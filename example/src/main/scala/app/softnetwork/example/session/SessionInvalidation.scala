package app.softnetwork.example.session

import app.softnetwork.session.SessionEndpoints._
import app.softnetwork.session.TapirSessionOptions._
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

object SessionInvalidationApp extends SessionInvalidation

trait SessionInvalidation extends SetSession {

  import system.dispatcher

  protected val logout: ServerEndpoint[Any, Future] =
    invalidateSession(refreshable, usingCookies) { // invalidate session using refreshable session continuity and cookies
      requiredSession(refreshable, usingCookies)
    }.post
      .in("logout")
      .out(stringBody)
      .serverLogicSuccess(session =>
        _ => {
          logger.info(s"Logging out $session")
          Future.successful("ok")
        }
      )

  override val endpoints: List[ServerEndpoint[Any, Future]] =
    List(
      login,
      logout
    )

}
