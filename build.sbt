val scala2_12 = "2.12.18"
val scala2_13 = "2.13.8"
val scala2 = List(scala2_12, scala2_13)

lazy val commonSettings = Seq(
  organization := "app.softnetwork.session",
  versionScheme := Some("early-semver")
)

val akkaHttpSession = "0.7.0"
val akkaHttpVersion = "10.2.7"
val akkaVersion = "2.6.18"
val json4sVersion = "4.0.4"
val akkaStreams = "com.typesafe.akka" %% "akka-stream" % akkaVersion
val akkaStreamsTestkit = "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % "test"
val tapirVersion = "1.3.0"

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11" % "test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true, name := "tapir-http-session", scalaVersion := scala2_13)
  .aggregate(core.projectRefs ++ example.projectRefs: _*)

lazy val example = (projectMatrix in file("example"))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      akkaStreams,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      "ch.qos.logback" % "logback-classic" % "1.2.10",
      "org.json4s" %% "json4s-ext" % json4sVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion
    )
  )
  .jvmPlatform(scalaVersions = scala2)
  .dependsOn(core)

lazy val core = (projectMatrix in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "com.softwaremill.akka-http-session" %% "core" % akkaHttpSession  excludeAll ExclusionRule(organization = "com.typesafe.akka"),
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      akkaStreams % "provided",
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server" % tapirVersion excludeAll ExclusionRule(organization = "com.typesafe.akka"),
      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test",
      akkaStreamsTestkit,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
      scalaTest
    )
  )
  .jvmPlatform(scalaVersions = scala2)
