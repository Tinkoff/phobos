libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.14.1" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.3.29" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
