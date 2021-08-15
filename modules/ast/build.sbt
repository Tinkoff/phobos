libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.15.4" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.5.6"  % "test",
  "org.typelevel"          %% "cats-core"       % "2.6.1",
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
