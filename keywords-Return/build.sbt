libraryDependencies += "com.lihaoyi" %%% "utest" % "0.8.3" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
enablePlugins(Example)

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.16" % Test
