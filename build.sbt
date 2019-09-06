import Publish._

name := "phobos"

scalaVersion := "2.12.8"

lazy val setModuleName   = moduleName := { s"phobos-${(publishName or name).value}" }
lazy val defaultSettings = List(setModuleName)

moduleName := "phobos"

lazy val core = project settings defaultSettings
lazy val utils = project settings defaultSettings dependsOn(core % "compile->compile;test->test;test->compile")
lazy val coreModules = Seq(core, utils)


lazy val phobos = project
  .in(file("."))
  .aggregate(coreModules.map(x => x: ProjectReference): _*)
  .dependsOn(coreModules.map(x => x: ClasspathDep[ProjectReference]): _*)