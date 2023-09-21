val dottyVersion = "3.0.1-RC1"
val effpiVersion = "0.0.3"

val useEffpiPlugin = settingKey[Boolean]("Use the effpi compiler plugin in sub-projects.")

inThisBuild( useEffpiPlugin := false )

lazy val effpi = (project in file("./effpi"))
  .settings(
    name := "effpi",
    version := effpiVersion,
    scalaVersion := dottyVersion,
  )

lazy val examples = project
  .in(file("effpi_sandbox"))
  .dependsOn(effpi)
  .settings(
    name := "effpi-sandbox",
    version := effpiVersion,
    scalaVersion := dottyVersion,
  )
