import scala.scalanative.build._

val commonSettings = Seq(
  version            := "0.6.1",
  scalaVersion       := "3.7.2",
  crossScalaVersions := Seq("3.7.2"),
  organization       := "ch.epfl.lara",
)

// Use your local Scala Native-enabled copy of silex.
// Adjust the path if needed.
lazy val silex = RootProject(file("../silex"))

lazy val scallion = project
  .in(file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    commonSettings,
    name := "scallion",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),

    ThisBuild / scalacOptions ++= Seq(
      "-source:3.7-migration",
      "-rewrite"
    ),

    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-root-content", baseDirectory.value + "/project/root-doc.txt"
    ),

    Compile / doc / target := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),

    nativeConfig ~= { config =>
      config
        .withMode(Mode.debug)
        .withLTO(LTO.none)
    },

    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))
  )
  .dependsOn(silex)

lazy val example = project
  .in(file("example"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    commonSettings,
    name := "scallion-examples",

    Compile / scalaSource := baseDirectory.value,

    nativeConfig ~= { config =>
      config
        .withMode(Mode.debug)
        .withLTO(LTO.none)
    }

    // Add this if example has a main:
    // Compile / mainClass := Some("your.example.Main")
  )
  .dependsOn(scallion)