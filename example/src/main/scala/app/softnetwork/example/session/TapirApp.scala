package app.softnetwork.example.session

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.softwaremill.example.session.MyScalaSession
import com.softwaremill.session.{
  InMemoryRefreshTokenStorage,
  RefreshTokenStorage,
  SessionConfig,
  SessionManager
}
import com.typesafe.scalalogging.StrictLogging
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.akkahttp.AkkaHttpServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import java.net.ServerSocket
import scala.concurrent.Future
import scala.io.StdIn

trait TapirApp extends StrictLogging {

  implicit val system: ActorSystem = ActorSystem("example")

  import system.dispatcher

  val sessionConfig: SessionConfig = SessionConfig.default(
    "c05ll3lesrinf39t7mc5h6un6r0c69lgfno69dsak3vabeqamouq4328cuaekros401ajdpkh60rrtpd8ro24rbuqmgtnd1ebag6ljnb65i8a55d482ok7o0nch0bfbe"
  )
  implicit val sessionManager: SessionManager[MyScalaSession] =
    new SessionManager[MyScalaSession](sessionConfig)
  implicit val refreshTokenStorage: RefreshTokenStorage[MyScalaSession] =
    new InMemoryRefreshTokenStorage[MyScalaSession] {
      def log(msg: String): Unit = logger.info(msg)
    }

  def endpoints: List[ServerEndpoint[Any, Future]]

  def swaggerEndpoints: List[ServerEndpoint[Any, Future]] =
    SwaggerInterpreter().fromEndpoints(endpoints.map(_.endpoint), "example", "v1.0")

  def routes: Route = AkkaHttpServerInterpreter().toRoute(endpoints ++ swaggerEndpoints)

  def main(args: Array[String]): Unit = {
    val port: Int = availablePort

    val bindingFuture = Http().newServerAt("localhost", port).bind(routes)

    println(
      s"Server started, press enter to stop. Visit http://localhost:$port/docs to see the swagger documentation."
    )
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete { _ =>
        system.terminate()
        println("Server stopped")
      }
  }

  def availablePort: Int = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort
    socket.close()
    port
  }
}
