// format: off

ThisBuild / name := "phobos"

ThisBuild / scalaVersion := "3.1.2"

lazy val commonDependencies =
  libraryDependencies ++=
    List(
      "com.fasterxml"  % "aalto-xml" % "1.3.2",
      "org.scalatest" %% "scalatest" % "3.2.13" % "test",
      "org.scalactic" %% "scalactic" % "3.2.13" % "test",
    ) ++
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => List(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
        )
        case Some((2, 13)) => List(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        )
        case _ => Nil
      })

def commonSettings(id: String) =
  Seq(
    name                    := id,
    moduleName              := s"phobos-$id",
    Compile / doc / sources := List.empty,
    scalacOptions ++= List(
      "-language:experimental.macros",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-feature",
      "-deprecation",
    ),
    scalacOptions ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _))  => List("-Xcheck-macros")
        case Some((2, 13)) => List("-Ymacro-annotations", "-Xlint")
        case Some((2, 12)) => List("-Ypartial-unification")
        case _ => Nil
      }
    )
  )

lazy val scala2Versions = List("2.12.17", "2.13.8")
lazy val scala3Versions = List("2.12.17", "2.13.8", "3.1.3")

lazy val `core` =
  (projectMatrix in file(s"modules/core"))
    .settings(commonSettings("core"))
    .settings(commonDependencies)
    .jvmPlatform(scala3Versions)

lazy val `core-3-0` =
  (projectMatrix in file(s"modules/core"))
    .settings(commonSettings("core-3-0"))
    .settings(
      target := file("modules/core/target30"),
      libraryDependencies ++= List(
        "com.fasterxml"  % "aalto-xml" % "1.3.2",
        "org.scalatest" %% "scalatest" % "3.2.11" % "test",
        "org.scalactic" %% "scalactic" % "3.2.11" % "test",
      )
    )
    .jvmPlatform(List("3.0.2"))

lazy val `akka-http` =
  (projectMatrix in file(s"modules/akka-http"))
    .settings(commonSettings("akka-http"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-stream" % "2.6.20" % "provided",
        "com.typesafe.akka" %% "akka-http"   % "10.2.10",
      )
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `akka-stream`   =   
  (projectMatrix in file(s"modules/akka-stream"))
    .settings(commonSettings("akka-stream"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-stream"  % "2.6.20",
        "com.typesafe.akka" %% "akka-testkit" % "2.6.20" % Test,
      )
    )
    .jvmPlatform(scala3Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `ast` =
  (projectMatrix in file(s"modules/ast"))
    .settings(commonSettings("ast"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "org.scalacheck"         %% "scalacheck"             % "1.16.0" % "test",
        "com.softwaremill.diffx" %% "diffx-scalatest-should" % "0.8.2"  % "test",
        "org.typelevel"          %% "cats-core"              % "2.8.0",
      ),
      Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3"),
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `cats` =
  (projectMatrix in file(s"modules/cats"))
    .settings(commonSettings("cats"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.8.0",
      ),
    )
    .jvmPlatform(scala3Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `derevo` =
  (projectMatrix in file(s"modules/derevo"))
    .settings(commonSettings("derevo"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "tf.tofu" %% "derevo-core" % "0.13.0",
      ),
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `enumeratum` =
  (projectMatrix in file(s"modules/enumeratum"))
    .settings(commonSettings("enumeratum"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "com.beachape" %% "enumeratum" % "1.7.0",
      ),
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `fs2` =
  (projectMatrix in file(s"modules/fs2"))
    .settings(commonSettings("fs2"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "co.fs2" %% "fs2-core" % "3.3.0",
        "co.fs2" %% "fs2-io"   % "3.3.0" % "test",
      ),
    )
    .jvmPlatform(scala3Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `fs2-ce2` =
  (projectMatrix in file(s"modules/fs2-ce2"))
    .settings(commonSettings("fs2-ce2"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "co.fs2" %% "fs2-core" % "2.5.11",
        "co.fs2" %% "fs2-io"   % "2.5.11" % "test",
      ),
    )
    .jvmPlatform(scala3Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `monix` =
  (projectMatrix in file(s"modules/monix"))
    .settings(commonSettings("monix"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "io.monix" %% "monix" % "3.4.1",
      ),
    )
    .jvmPlatform(scala3Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val `refined` =
  (projectMatrix in file(s"modules/refined"))
    .settings(commonSettings("refined"))
    .settings(
      commonDependencies,
      libraryDependencies ++= Seq(
        "eu.timepit" %% "refined" % "0.10.1",
      ),
    )
    .jvmPlatform(scala2Versions)
    .dependsOn(core % "compile->compile;test->test")

lazy val modules: Seq[ProjectReference] =
  Seq(
    `core`.projectRefs,
    `core-3-0`.projectRefs,
    `akka-http`.projectRefs,
    `akka-stream`.projectRefs,
    `ast`.projectRefs,
    `cats`.projectRefs,
    `derevo`.projectRefs,
    `enumeratum`.projectRefs,
    `fs2`.projectRefs,
    `fs2-ce2`.projectRefs,
    `monix`.projectRefs,
    `refined`.projectRefs,
  ).flatten

lazy val phobos = project
  .in(file("."))
  .settings(
    moduleName         := "phobos",
    publish / skip     := true,
  )
  .aggregate(modules: _*)