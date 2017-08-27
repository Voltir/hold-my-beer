import Dependencies._


lazy val commonSettings = Seq(
  scalaVersion := "2.11.11",
  version := "0.1.0-SNAPSHOT"
)

lazy val internal = (project in file("internal"))
  .settings(commonSettings)
  .settings(

    libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0",
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.17.1",
    //main dependencies
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    //test dependencies
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "sourcecode" % "0.1.4" % Test,
      scalaTest % Test
    )
  )

//todo delete this, this does not need to be multi-project
lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies += scalaTest % Test
  ).dependsOn(internal)
