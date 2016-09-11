lazy val root = (project in file(".")).enablePlugins(PlayScala).settings(
  name := "goi",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    ws,
    "com.h2database" % "h2" % "1.4.192",
    "com.lihaoyi" %% "scalatags" % "0.5.5",
    "com.typesafe.play" %% "play-slick" % "2.0.0"
  )
)
