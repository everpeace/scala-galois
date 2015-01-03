import sbt._
import Keys._

object ScalaGaloisBuild extends Build {

  lazy val root =
    project(id = "scala-galois",
      base = file(".")) aggregate(core, secret_share, example)

  import Shared.Deps._

  lazy val core = project(
    id       = "core",
    base     = file("core"),
    settings = Seq(
      libraryDependencies ++= Seq(specs2)
    )
  )

  lazy val secret_share = project(
    id       = "secret-share",
    base     = file("secret-share"),
    settings = Seq(
      libraryDependencies ++= Seq(akka, scalacheck, specs2)
    )
  ) dependsOn (core)

  lazy val example = project(
    id   = "example",
    base = file("example")
  ) dependsOn (core)

  def project(id: String, base: File, settings: Seq[Def.Setting[_]] = Nil) =
    Project(
      id       = id,
      base     = base,
      settings = Defaults.coreDefaultSettings ++ Shared.settings ++ settings
    )
}

object Shared {
  object V {
//    val spire = "0.9.0"
    val akka = "2.3.4"
    val scalacheck = "1.12.1"
    val specs2 = "2.4.15"
  }

  object Deps {
//    val spire = "org.spire-math" %% "spire" % V.spire
    val akka = "com.typesafe.akka" %% "akka-actor" % V.akka
    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test"
    val specs2 = "org.specs2" %% "specs2-core" % V.specs2 % "test"
  }

  val settings = Seq(
    organization := "com.github.everpeace",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.4"),
    scalacOptions := Seq(
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps",
      "-unchecked"),
    resolvers ++= Seq("Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
                      "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
                      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"),
    initialCommands := "import galois._"
  )

}
