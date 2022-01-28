libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.4.0-M10"

libraryDependencies += "com.thoughtworks.tryt" %%% "invariant" % "3.0.0" % Test

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.4.0-M10" % Test

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.11" % Test

enablePlugins(Example)

import scala.meta._
exampleSuperTypes += init"_root_.org.scalatest.Inside"
