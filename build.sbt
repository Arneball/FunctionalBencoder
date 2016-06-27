name := """Bencoder"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.9" % "test"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.6.6"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"



// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

import scalariform.formatter.preferences._

scalariformSettings

ScalariformKeys.preferences := List(
    AlignSingleLineCaseStatements,
    RewriteArrowSymbols,
    DoubleIndentClassDeclaration,
    PreserveDanglingCloseParenthesis
).foldLeft(ScalariformKeys.preferences.value) { _.setPreference(_, true) }
