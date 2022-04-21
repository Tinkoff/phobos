libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.16.0" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.7.1"  % "test",
  "org.typelevel"          %% "cats-core"       % "2.7.0",
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
