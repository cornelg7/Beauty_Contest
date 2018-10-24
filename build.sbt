enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "Beauty Contest"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "com.lihaoyi" %%% "scalatags" % "0.6.1"
)
