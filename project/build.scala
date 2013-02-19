import sbt._
import Keys._

object ScalagaloisBuild extends sbt.Build {

  lazy val root =
    project(id = "scala-galois",
            base = file(".")) aggregate(core, example)

  lazy val core =
    project(id = "core",
            base = file("core"),
            settings = Seq(
              libraryDependencies <++= scalaVersion (v => Seq(
                Shared.specsDep(v):_*
            ))))

  lazy val example =
    project(id = "example",
      base = file("example"),
      settings = Seq(
        libraryDependencies <++= scalaVersion (v => Seq(
          Shared.specsDep(v):_*
        )))) dependsOn(core)

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
    resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
    initialCommands := "import galois._"
  )
  
}
