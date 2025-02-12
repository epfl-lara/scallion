val commonSettings = Seq(
  version            := "0.6.1",
  scalaVersion       := "3.5.2",
  crossScalaVersions := Seq("3.5.2"),
  organization       := "ch.epfl.lara",
//  resolvers          += "bintray-epfl-lara" at "https://dl.bintray.com/epfl-lara/maven",
)

def ghProject(repo: String, version: String) = RootProject(uri(s"${repo}#${version}"))

lazy val silex = ghProject("https://github.com/epfl-lara/silex.git", "f13df9ee24288cee167e262b6a36be29c63b7045")

lazy val scallion = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "scallion",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),

    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-root-content", baseDirectory.value + "/project/root-doc.txt"
    ),

    Compile / doc / target := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    ),

    bintrayOrganization := Some("epfl-lara"),
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    bintrayPackageLabels := Seq(
      "scala", "parser", "parsing",
      "ll1", "ll1-parsing", "ll1-grammar",
      "parser-combinators", "parsing-combinators"
    ),
  ).dependsOn(silex)

lazy val example = project
  .in(file("example"))
  .settings(
    commonSettings,
    name := "scallion-examples",
    Compile / scalaSource := baseDirectory.value,
  )
  .dependsOn(scallion)

/*
lazy val benchmark = project
  .in(file("benchmark"))
  .settings(
    commonSettings,
    name                   := "scallion-benchmarks",
    fork in run            := true,
    run / baseDirectory    := file("."),
    javaOptions in run     += "-Xss1024K",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test    := baseDirectory.value / "src",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases",
    resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "com.codecommit" %% "parseback-core" % "0.3",
    libraryDependencies += "com.codecommit" %% "parseback-cats" % "0.3",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
  )
  .dependsOn(scallion)
 */


