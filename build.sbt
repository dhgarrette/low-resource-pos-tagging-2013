import com.typesafe.sbt.SbtStartScript

import AssemblyKeys._ // put this at the top of the file

name := "low-resource-pos-tagging-2013"

version := "0.0.1"

scalaVersion := "2.10.1"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "OpenNLP repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % "7.0.0",
  "com.typesafe" % "scalalogging-log4j_2.10" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3",
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default")

// Junto Dependencies
libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",
  "org.clapper" % "argot_2.10" % "1.0.0",
  "net.sf.trove4j" % "trove4j" % "3.0.3",
  "com.typesafe" % "scalalogging-log4j_2.10" % "1.0.1")

seq(assemblySettings: _*)

seq(SbtStartScript.startScriptForClassesSettings: _*)

SbtStartScript.stage in Compile := Unit

test in assembly := {}

scalacOptions ++= Seq("-deprecation")

