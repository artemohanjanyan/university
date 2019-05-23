name := "rx"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.mongodb" % "mongodb-driver-rx" % "1.5.0",
  "io.reactivex" %% "rxscala" % "0.26.5",
  "io.netty" % "netty-all" % "4.1.22.Final",
  "io.reactivex" % "rxnetty-http" % "0.5.2",
  "io.reactivex" % "rxnetty-common" % "0.5.2",
  "io.reactivex" % "rxnetty-tcp" % "0.5.2",
  "com.lambdista" %% "money" % "0.6.2"
)