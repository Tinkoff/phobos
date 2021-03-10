import sbt.SettingKey

object Publish {
  val publishVersion: SettingKey[String] = SettingKey(
    label = "publishVersion",
    description = "version prefix, it will be *the* version of module if branch is master",
  )
}
