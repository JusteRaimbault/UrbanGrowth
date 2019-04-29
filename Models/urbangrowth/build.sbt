scalaVersion := "2.12.6"

name := "urbangrowth"

version := "0.1-SNAPSHOT"

//mainClass in (Compile, run) := Some("urbangrowth.Run")
mainClass in (Compile, run) := Some("urbangrowth.test.Test")

enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("urbangrowth.*")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""


resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenCentral,
  Resolver.file("Local ivy", file( (new java.io.File("~")).getAbsolutePath+"/.ivy2"))(Resolver.ivyStylePatterns)
)


libraryDependencies ++= Seq(
  "gov.nist.math" % "jama" % "1.0.3",
  "org.openmole" % "spatialdata_2.12" % "0.1-SNAPSHOT"
)

//libraryDependencies += "org.geotools" % "gt-referencing" % "9.3"