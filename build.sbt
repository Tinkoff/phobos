ThisBuild / name := "phobos"

ThisBuild / scalaVersion := "2.13.1"

lazy val supportedVersions = List("2.12.10", "2.13.1")

lazy val commonDependencies =
  libraryDependencies ++= List(
    "org.typelevel" %% "cats-core" % "2.1.1",
    "com.fasterxml" % "aalto-xml" % "1.2.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalactic" %% "scalactic" % "3.1.1" % "test",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  )

def configuration(id: String)(project: Project): Project =
  project.settings(
    moduleName := s"phobos-$id",
    crossScalaVersions := supportedVersions,
    sources in (Compile, doc) := List.empty,
    commonDependencies,
    scalacOptions ++= List(
      "-language:experimental.macros",
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Nil
        case _ => List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => List("-Ymacro-annotations")
        case _ => Nil
      }
    }
  )

def phobosModule(id: String) =
  Project(id, file(s"modules/$id"))
    .configure(configuration(id))

lazy val core = phobosModule("core")
lazy val derevo = phobosModule("derevo") dependsOn (core % "compile->compile;test->test")
lazy val enumeratum = phobosModule("enumeratum") dependsOn (core % "compile->compile;test->test")
lazy val `akka-http` = phobosModule("akka-http") dependsOn (core % "compile->compile;test->test")
lazy val `akka-stream` = phobosModule("akka-stream") dependsOn (core % "compile->compile;test->test")
lazy val monix = phobosModule("monix") dependsOn (core % "compile->compile;test->test")
lazy val fs2 = phobosModule("fs2") dependsOn (core % "compile->compile;test->test")
lazy val ast = phobosModule("ast") dependsOn (core % "compile->compile;test->test")

lazy val modules: List[ProjectReference] = List(core, `akka-http`, derevo, enumeratum, monix, fs2, `akka-stream`, ast)


lazy val phobos = project
  .in(file("."))
  .settings(
    moduleName := "phobos",
    skip in publish := true
  )
  .aggregate(modules: _*)