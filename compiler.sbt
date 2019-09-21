scalaVersion in ThisBuild := "2.12.8"

val options = Seq(
  "-Ypartial-unification",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-Ywarn-unused:imports",
  //  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  //  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  //  "-Ywarn-unused:params:off",          // Warn if a value parameter is unused.
  //  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  //  "-Ywarn-unused:privates",            // Warn if a private member is unused.
//  "-Xfatal-warnings",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  //  "-Ystatistics",
)
scalacOptions in ThisBuild ++= options

libraryDependencies in ThisBuild ++= Seq(
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
)