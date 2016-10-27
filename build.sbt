lazy val commonSettings = Seq(
  organization := "io.sysmic",
  version := "1.0",
  scalaVersion := "2.11.8",
  unmanagedBase := (unmanagedBase in ThisProject).value,
  unmanagedJars in Compile <++= baseDirectory map { base =>
    val libs = base / "lib"
    (libs ** "*.jar").classpath
  }
)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(Seq(
    libraryDependencies := Seq(
      "com.google.guava" % "guava" % "19.0",
      "com.google.code.gson" % "gson" % "2.7",
      "com.google.protobuf" % "protobuf-java" % "3.0.0",
      "junit" % "junit" % "4.12",
      "org.scalatest" %% "scalatest" % "3.0.0"
    )
  ))

lazy val index = (project in file("index"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val benchmark = (project in file("benchmark"))
  .enablePlugins(JmhPlugin)
  .settings(commonSettings: _*)
  .settings(pl.project13.scala.sbt.SbtJmh.projectSettings)
  .dependsOn(core, index)
