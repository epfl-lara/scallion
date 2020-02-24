
val commonSettings = Seq(
  version            := "0.4",
  scalaVersion       := "2.13.1",
  crossScalaVersions := Seq("2.12.8", "2.13.1"),
  organization       := "ch.epfl.lara",
)

lazy val scallion = project
  .in(file("."))
  .settings(
    commonSettings,
    name               := "scallion",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),

    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-source-url", "https://raw.githubusercontent.com/epfl-lara/scallion/master€{FILE_PATH}.scala",
      "-doc-root-content", baseDirectory.value + "/project/root-doc.txt"
    ),

    target in Compile in doc := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    ),

    bintrayOrganization := Some("epfl-lara"),
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    bintrayPackageLabels := Seq(
      "scala", "parser", "parsing",
      "ll1", "ll1-parsing", "ll1-grammar",
      "parser-combinators", "parsing-combinators"
    ),
  )

lazy val example = project
  .in(file("example"))
  .settings(
    commonSettings,
    name := "scallion-examples",
    scalaSource in Compile := baseDirectory.value,
  )
  .dependsOn(scallion)

