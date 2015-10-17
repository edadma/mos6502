name := "mos6502"

version := "0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.1" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.2"

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value + ".Main" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value + ".Main" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

seq(bintraySettings:_*)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>)
