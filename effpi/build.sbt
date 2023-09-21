val dottyVersion = "3.0.1-RC1"
val effpiVersion = "0.0.3"

val useEffpiPlugin = settingKey[Boolean]("Use the effpi compiler plugin in sub-projects.")

inThisBuild(
  // Can be changed from sbt by running `set ThisBuild / useEffpiPlugin := false`
  useEffpiPlugin := true
)

lazy val effpi = (project in file(".")).
  settings(
    name := "effpi",
    version := effpiVersion,

    scalaVersion := dottyVersion,
    //addCompilerPlugin("uk.ac.ic" %% "effpi-verifier" % "0.0.3"),
  )
