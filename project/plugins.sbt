addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.0")

val sbtSoftNetworkVersion = "0.1.7"
resolvers += "Softnetwork releases" at "https://softnetwork.jfrog.io/artifactory/releases/"
addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-git" % sbtSoftNetworkVersion)
addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-info" % sbtSoftNetworkVersion)
addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-publish" % sbtSoftNetworkVersion)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.8")
