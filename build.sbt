scalaVersion := "2.12.4"

scalacOptions in (Compile, doc) ++= Seq(
  "-groups"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"