ThisBuild / name := "phobos"

ThisBuild / scalaVersion := "2.12.8"

lazy val commonDependencies =
  libraryDependencies ++= Seq(
    "org.typelevel" % "cats-core_2.12" % "2.0.0",
    "com.fasterxml" % "aalto-xml" % "1.2.1",
    "org.scalactic" %% "scalactic" % "3.0.5" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  )

def configuration(id: String)(project: Project): Project =
  project.settings(
    moduleName := s"phobos-$id",
    sources in (Compile, doc) := Seq.empty,
    commonDependencies
  )

def phobosModule(id: String) =
  Project(id, file(s"modules/$id"))
    .configure(configuration(id))

lazy val core = phobosModule("core")
lazy val derevo = phobosModule("derevo") dependsOn (core % "compile->compile;test->test")
lazy val enumeratum = phobosModule("enumeratum") dependsOn (core % "compile->compile;test->test")
lazy val akka = phobosModule("akka") dependsOn (core % "compile->compile;test->test")

lazy val modules: Seq[ProjectReference] = Seq(core, akka, derevo, enumeratum)


lazy val phobos = project
  .in(file("."))
  .settings(moduleName := "phobos")
  .aggregate(modules: _*)