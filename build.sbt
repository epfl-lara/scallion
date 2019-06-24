scalaVersion := "2.12.7"

name := "scallion"

version := "0.1.1"

organization := "ch.epfl.lara"

scalacOptions in (Compile, doc) ++= Seq(
  "-groups"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

bintrayOrganization := Some("epfl-lara")

licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))

bintrayPackageLabels := Seq("scala", "parser", "parsing",
                            "ll1", "ll1-parsing", "ll1-grammar",
                            "parser-combinators", "parsing-combinators")