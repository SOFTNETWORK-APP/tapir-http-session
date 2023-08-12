package com.softwaremill.session

import akka.http.scaladsl.model.{DateTime, HttpHeader}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.softwaremill.session.SessionOptions._
import com.softwaremill.session.TestData._

trait MultipleTransportTest { this: ScalatestRouteTest =>

  trait TestUsingTransport {
    def transportName: String

    def getSession: Option[String]
    def countSessionHeaders: Int
    def setSessionHeader(s: String): HttpHeader
    def isSessionExpired: Boolean

    def getRefreshToken: Option[String]
    def countRefreshTokenHeaders: Int
    def setRefreshTokenHeader(s: String): HttpHeader
    def isRefreshTokenExpired: Boolean

    def getSessionTransport: GetSessionTransport
    def setSessionTransport: SetSessionTransport
  }

  object TestUsingCookies extends TestUsingTransport {
    val sessionCookieName: String = sessionConfig.sessionCookieConfig.name
    val refreshTokenCookieName: String = sessionConfig.refreshTokenCookieConfig.name

    val transportName = "cookies"

    def cookiesMap: Map[String, HttpCookie] =
      headers.collect { case `Set-Cookie`(cookie) => cookie.name -> cookie }.toMap
    private def countCookies(name: String): Int = headers.count {
      case `Set-Cookie`(cookie) => cookie.name == name
      case _                    => false
    }

    def getSession: Option[String] = cookiesMap.get(sessionCookieName).map(_.value)
    def countSessionHeaders: Int = countCookies(sessionCookieName)
    def setSessionHeader(s: String): Cookie = Cookie(sessionCookieName, s)
    def isSessionExpired: Boolean =
      cookiesMap.get(sessionCookieName).flatMap(_.expires).contains(DateTime.MinValue)

    def getRefreshToken: Option[String] = cookiesMap.get(refreshTokenCookieName).map(_.value)
    def countRefreshTokenHeaders: Int = countCookies(refreshTokenCookieName)
    def setRefreshTokenHeader(s: String): Cookie = Cookie(refreshTokenCookieName, s)
    def isRefreshTokenExpired: Boolean =
      cookiesMap.get(refreshTokenCookieName).flatMap(_.expires).contains(DateTime.MinValue)

    def getSessionTransport: GetSessionTransport = usingCookies
    def setSessionTransport: SetSessionTransport = usingCookies
  }

  object TestUsingHeaders extends TestUsingTransport {
    val setSessionHeaderName: String = sessionConfig.sessionHeaderConfig.sendToClientHeaderName
    val sessionHeaderName: String = sessionConfig.sessionHeaderConfig.getFromClientHeaderName
    val setRefreshTokenHeaderName: String =
      sessionConfig.refreshTokenHeaderConfig.sendToClientHeaderName
    val refreshTokenHeaderName: String =
      sessionConfig.refreshTokenHeaderConfig.getFromClientHeaderName

    val transportName = "headers"

    private def countHeaders(name: String): Int = headers.count(_.is(name.toLowerCase))

    def getSession: Option[String] = header(setSessionHeaderName).map(_.value)
    def countSessionHeaders: Int = countHeaders(setSessionHeaderName)
    def setSessionHeader(s: String): RawHeader = RawHeader(sessionHeaderName, s)
    def isSessionExpired: Boolean = getSession.contains("")

    def getRefreshToken: Option[String] = header(setRefreshTokenHeaderName).map(_.value)
    def countRefreshTokenHeaders: Int = countHeaders(setRefreshTokenHeaderName)
    def setRefreshTokenHeader(s: String): RawHeader = RawHeader(refreshTokenHeaderName, s)
    def isRefreshTokenExpired: Boolean = getRefreshToken.contains("")

    def getSessionTransport: GetSessionTransport = usingHeaders
    def setSessionTransport: SetSessionTransport = usingHeaders
  }
}
