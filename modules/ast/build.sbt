libraryDependencies ++= Seq(
  "org.scalacheck"         %% "scalacheck"      % "1.15.3" % "test",
  "com.softwaremill.diffx" %% "diffx-scalatest" % "0.4.5"  % "test",
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
