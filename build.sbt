Global / onChangedBuildSource := ReloadOnSourceChanges

val dottyVersion = "3.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "euterpea-scala3",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      ("org.smop" %% "smop-sound" % "0.1.0-SNAPSHOT").withDottyCompat(scalaVersion.value),
      ("org.scodec" %% "scodec-bits" % "1.1.12").withDottyCompat(scalaVersion.value)
    )
  )
