import Publish._

publishVersion := "0.9.1"

ThisBuild / organization := "ru.tinkoff"
ThisBuild / version := {
  val branch = git.gitCurrentBranch.value
  if (branch == "master") publishVersion.value
  else s"${publishVersion.value}-$branch-SNAPSHOT"
}

ThisBuild / publishMavenStyle := true

ThisBuild / publishTo :=
  (if (!isSnapshot.value) {
    sonatypePublishToBundle.value
  } else {
    Some(Opts.resolver.sonatypeSnapshots)
  })

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/TinkoffCreditSystems/phobos"),
    "git@github.com:TinkoffCreditSystems/phobos"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "valentiay",
    name = "Alexander Valentinov",
    email = "a.valentinov@tinkoff.ru",
    url = url("https://github.com/valentiay")
  )
)

ThisBuild / description := "Fast xml data binding library"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/TinkoffCreditSystems/phobos"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
