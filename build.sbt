ThisBuild / name := "phobos"

ThisBuild / scalaVersion := "2.13.7"

lazy val supportedVersions = List("2.12.15", "2.13.6")

lazy val commonDependencies =
  libraryDependencies ++= List(
    "com.fasterxml"  % "aalto-xml"     % "1.3.1",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest"     % "3.2.11" % "test",
    "org.scalactic" %% "scalactic"     % "3.2.10" % "test",
  )

def onScalaVersion[B](`on-2-12`: => B, `on-2-13`: => B): Def.Initialize[B] =
  Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => `on-2-13`
      case _             => `on-2-12`
    }
  }

def configuration(id: String)(project: Project): Project =
  project.settings(
    moduleName              := s"phobos-$id",
    crossScalaVersions      := supportedVersions,
    Compile / doc / sources := List.empty,
    commonDependencies,
    scalacOptions ++= List(
      "-language:experimental.macros",
    ) ++ onScalaVersion(
      `on-2-13` = Nil,
      `on-2-12` = List("-Ypartial-unification"),
    ).value,
    libraryDependencies ++= {
      onScalaVersion(
        `on-2-13` = Nil,
        `on-2-12` = List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)),
      ).value
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => List("-Ymacro-annotations")
        case _             => Nil
      }
    },
  )

def phobosModule(id: String) =
  Project(id, file(s"modules/$id"))
    .configure(configuration(id))

lazy val core          = phobosModule("core")
lazy val cats          = phobosModule("cats") dependsOn (core        % "compile->compile;test->test")
lazy val derevo        = phobosModule("derevo") dependsOn (core      % "compile->compile;test->test")
lazy val enumeratum    = phobosModule("enumeratum") dependsOn (core  % "compile->compile;test->test")
lazy val `akka-http`   = phobosModule("akka-http") dependsOn (core   % "compile->compile;test->test")
lazy val `akka-stream` = phobosModule("akka-stream") dependsOn (core % "compile->compile;test->test")
lazy val monix         = phobosModule("monix") dependsOn (core       % "compile->compile;test->test")
lazy val fs2           = phobosModule("fs2") dependsOn (core         % "compile->compile;test->test")
lazy val `fs2-ce2`     = phobosModule("fs2-ce2") dependsOn (core     % "compile->compile;test->test")
lazy val refined       = phobosModule("refined") dependsOn (core     % "compile->compile;test->test")
lazy val ast           = phobosModule("ast") dependsOn (core         % "compile->compile;test->test")

lazy val modules: List[ProjectReference] =
  List(core, `akka-http`, derevo, enumeratum, monix, fs2, `fs2-ce2`, `akka-stream`, refined, ast)

lazy val phobos = project
  .in(file("."))
  .settings(
    moduleName     := "phobos",
    publish / skip := true,
  )
  .aggregate(modules: _*)
