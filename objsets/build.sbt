submitProjectName := "objsets"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-Yinline-warnings"
)

fork := true

javaOptions += "-Xmx2G"

parallelExecution in Test := false


// See documentation in ProgFunBuild.scala
projectDetailsMap := {
val depsQuickcheck = Seq(
    "org.scalacheck" %% "scalacheck" % "1.12.1"
  )
}
