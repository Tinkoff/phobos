libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.15.2" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.4.4"  % "test",
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
