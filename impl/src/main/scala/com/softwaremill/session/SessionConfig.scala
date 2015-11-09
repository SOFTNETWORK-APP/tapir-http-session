package com.softwaremill.session

import java.util.concurrent.TimeUnit

import com.typesafe.config.{ConfigValueFactory, ConfigFactory, Config}

case class CookieConfig(
  name: String,
  domain: Option[String],
  path: Option[String],
  maxAge: Option[Long],
  secure: Boolean,
  httpOnly: Boolean
)

case class HeaderConfig(
  sendToClientHeaderName: String,
  getFromClientHeaderName: String
)

case class SessionConfig(
  /**
   * Should be different on each environment and **kept secret!**. It's used to sign and encrypt cookie data.
   * This should be a long random string.
   */
  serverSecret: String,
  /**
   * For session cookies, **do not** set the [[CookieConfig.maxAge]], as this will turn it into a persistent cookie
   * (on the client).
   */
  sessionCookieConfig: CookieConfig,
  sessionHeaderConfig: HeaderConfig,
  /**
   * If you'd like session cookies to expire as well after a period of inactivity, you can optionally include an
   * expiration date in the cookie data (expiration will be validated on the server). The expiration date will be
   * calculated by adding the given number of seconds to the time at which the session is last updated.
   */
  sessionMaxAgeSeconds: Option[Long],
  /**
   * By default the session data won't be encrypted, only signed with a hash. Set this to true if you'd like the data
   * to be encrypted using a symmetrical key.
   */
  sessionEncryptData: Boolean,
  csrfCookieConfig: CookieConfig,
  /**
   * Name of the header or form field in which the CSRF token will be submitted.
   */
  csrfSubmittedName: String,
  refreshTokenCookieConfig: CookieConfig,
  refreshTokenHeaderConfig: HeaderConfig,
  /**
   * When a refresh token is used to log in, a new one is generated. The old one should be deleted with a delay,
   * to properly serve concurrent requests using the old token.
   */
  removeUsedRefreshTokenAfter: Long
)

object SessionConfig {
  private implicit class PimpedConfig(config: Config) {
    val noneValue = "none"

    def getOptionalString(path: String) = if (config.getAnyRef(path) == noneValue) None else
      Some(config.getString(path))
    def getOptionalLong(path: String) = if (config.getAnyRef(path) == noneValue) None else
      Some(config.getLong(path))
    def getOptionalDurationSeconds(path: String) = if (config.getAnyRef(path) == noneValue) None else
      Some(config.getDuration(path, TimeUnit.SECONDS))
  }

  def fromConfig(config: Config = ConfigFactory.load()): SessionConfig = {
    val scopedConfig = config.getConfig("akka.http.session")
    val csrfConfig = scopedConfig.getConfig("csrf")
    val refreshTokenConfig = scopedConfig.getConfig("refresh-token")

    SessionConfig(
      serverSecret = scopedConfig.getString("server-secret"),
      sessionCookieConfig = CookieConfig(
        name = scopedConfig.getString("cookie.name"),
        domain = scopedConfig.getOptionalString("cookie.domain"),
        path = scopedConfig.getOptionalString("cookie.path"),
        maxAge = scopedConfig.getOptionalDurationSeconds("cookie.max-age"),
        secure = scopedConfig.getBoolean("cookie.secure"),
        httpOnly = scopedConfig.getBoolean("cookie.http-only")
      ),
      sessionHeaderConfig = HeaderConfig(
        sendToClientHeaderName = scopedConfig.getString("header.send-to-client-name"),
        getFromClientHeaderName = scopedConfig.getString("header.get-from-client-name")
      ),
      sessionMaxAgeSeconds = scopedConfig.getOptionalLong("max-age-seconds"),
      sessionEncryptData = scopedConfig.getBoolean("encrypt-data"),
      csrfCookieConfig = CookieConfig(
        name = csrfConfig.getString("cookie.name"),
        domain = csrfConfig.getOptionalString("cookie.domain"),
        path = csrfConfig.getOptionalString("cookie.path"),
        maxAge = csrfConfig.getOptionalDurationSeconds("cookie.max-age"),
        secure = csrfConfig.getBoolean("cookie.secure"),
        httpOnly = csrfConfig.getBoolean("cookie.http-only")
      ),
      csrfSubmittedName = csrfConfig.getString("submitted-name"),
      refreshTokenCookieConfig = CookieConfig(
        name = refreshTokenConfig.getString("cookie.name"),
        domain = refreshTokenConfig.getOptionalString("cookie.domain"),
        path = refreshTokenConfig.getOptionalString("cookie.path"),
        maxAge = refreshTokenConfig.getOptionalDurationSeconds("cookie.max-age"),
        secure = refreshTokenConfig.getBoolean("cookie.secure"),
        httpOnly = refreshTokenConfig.getBoolean("cookie.http-only")
      ),
      refreshTokenHeaderConfig = HeaderConfig(
        sendToClientHeaderName = refreshTokenConfig.getString("header.send-to-client-name"),
        getFromClientHeaderName = refreshTokenConfig.getString("header.get-from-client-name")
      ),
      removeUsedRefreshTokenAfter = refreshTokenConfig.getDuration("remove-used-token-after", TimeUnit.SECONDS)
    )
  }

  /**
   * Creates a default configuration using the given secret.
   */
  def default(serverSecret: String) = fromConfig(ConfigFactory.load()
    .withValue("akka.http.session.server-secret", ConfigValueFactory.fromAnyRef(serverSecret)))
}
