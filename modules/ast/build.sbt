libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.15.4" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.5.0"  % "test",
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
