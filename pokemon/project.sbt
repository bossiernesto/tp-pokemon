name := "Pokemon"

description := "Small implementation of a Lisp interpreter/compiler in Scala"

scalaVersion := "2.11.4"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val cacao = FDProject(
	"org.scalatest" %% "scalatest" % "2.2.1" % "test"
)


///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"
