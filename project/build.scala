import sbt._
import Keys._

object ScalagaloisBuild extends sbt.Build {

  lazy val root =
    project(id = "scala-galois",
      base = file(".")) aggregate(core, secret_share, example)

  lazy val core =
    project(id = "core",
      base = file("core"),
      settings = Seq(
        libraryDependencies <++= scalaVersion(v => Seq(
          Shared.specsDep(v): _*
        ))))

  lazy val secret_share =
    project(id = "secret-share",
      base = file("secret-share"),
      settings = Seq(
        libraryDependencies <++= scalaVersion(v => Seq("com.typesafe.akka" % "akka-actor" % "2.0.5" withSources())
          ++ Seq("org.scalacheck" %% "scalacheck" % "1.10.0" % "test") ++ Seq(
          Shared.specsDep(v): _*
        )))) dependsOn (core)

  lazy val example =
    project(id = "example",
      base = file("example"),
      settings = Seq(
        libraryDependencies <++= scalaVersion(v => Seq(
          Shared.specsDep(v): _*
        )))) dependsOn (core)

  def project(id: String, base: File, settings: Seq[Project.Setting[_]] = Nil) =
    Project(id = id,
      base = base,
      settings = Project.defaultSettings ++ Shared.settings ++ settings)
}

object Shared {

  /** Resolve specs version for the current scala version (thanks @n8han). */
  def specsDep(sv: String, cfg: String = "test") =
    (sv.split("[.-]").toList match {
      case "2" :: "8" :: "1" :: Nil =>
        "org.specs2" %% "specs2" % "1.5" ::
          "org.specs2" %% "specs2-scalaz-core" % "5.1-SNAPSHOT" ::
          Nil
      case "2" :: "9" :: "0" :: _ => "org.specs2" % "specs2_2.9.1" % "1.7.1" :: Nil
      case "2" :: "9" :: _ :: _ => "org.specs2" % "specs2_2.9.1" % "1.8.2" :: Nil
      case _ => sys.error("Specs not supported for scala version %s" format sv)
    }) map (_ % cfg)

  val settings = Seq(
    organization := "com.github.everpeace",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.8.1", "2.9.0", "2.9.0-1", "2.9.1"),
    resolvers ++= Seq("Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
                      "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
                      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"),
    initialCommands := "import galois._"
  )

}
