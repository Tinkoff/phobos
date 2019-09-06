import Publish._

libraryDependencies ++= Seq(
  "org.typelevel" % "cats-core_2.12" % "2.0.0-RC1",
  "io.monix" %% "monix" % "3.0.0-RC4",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19" % "provided",
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.fasterxml" % "aalto-xml" % "1.2.1",
  "org.scalactic" %% "scalactic" % "3.0.5" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.manatki" % "derevo-core_2.12" % "0.10.1",
  "com.beachape" %% "enumeratum" % "1.5.13",
)

publishName := "utils"

sources in (Compile, doc) := Seq.empty