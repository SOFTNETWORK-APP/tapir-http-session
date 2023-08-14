package app.softnetwork.example.session

import app.softnetwork.session.TapirSessionOptions._
import com.softwaremill.session.SessionResult._
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

object VariousSessionsApp extends VariousSessions

trait VariousSessions extends SessionInvalidation {

  import system.dispatcher

  private[this] val secret: ServerEndpoint[Any, Future] = {
    requiredSession(oneOff, usingCookies).get
      .in("secret")
      .out(stringBody)
      .serverLogicSuccess(_ => _ => Future.successful("treasure"))
  }

  private[this] val open: ServerEndpoint[Any, Future] =
    optionalSession(oneOff, usingCookies).get
      .in("open")
      .out(stringBody)
      .serverLogicSuccess(_ => _ => Future.successful("small treasure"))

  private[this] val detail: ServerEndpoint[Any, Future] =
    session(oneOff, usingCookies, None).get
      .in("detail")
      .out(stringBody)
      .serverLogicSuccess(sessionResult =>
        _ =>
          Future.successful {
            sessionResult match {
              case Decoded(_)          => "decoded"
              case DecodedLegacy(_)    => "decoded legacy"
              case CreatedFromToken(_) => "created from token"
              case NoSession           => "no session"
              case TokenNotFound       => "token not found"
              case Expired             => "expired"
              case Corrupt(_)          => "corrupt"
            }
          }
      )

  override val endpoints: List[ServerEndpoint[Any, Future]] =
    List(
      login,
      logout,
      secret,
      open,
      detail
    )

}
