name := """Bencoder"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

// Change this to another test framework if you prefer
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.0" % "test"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.3.0" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"



// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

import scalariform.formatter.preferences._

scalariformPreferences := List(
    AlignSingleLineCaseStatements,
    RewriteArrowSymbols,
    DoubleIndentClassDeclaration/*,
    PreserveDanglingCloseParenthesis*/
).foldLeft(scalariformPreferences.value) { _.setPreference(_, true) }
