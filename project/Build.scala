import sbt._, Keys._

object VindiniumBot extends Build {

  lazy val bot = Project(
    id = "vindinium-bot",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      fork := true,
      organization := "org.jousse",
      version := "0.1",
      scalaVersion := "2.10.3",
      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.2.1",
        "org.scalaj" %% "scalaj-http" % "0.3.12"
      ),
      scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked"))
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
