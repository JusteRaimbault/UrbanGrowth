scalaVersion := "2.12.6"
//scalaVersion := "2.11.8"

name := "urbangrowth"

version := "0.1-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("staging")
resolvers += Resolver.mavenCentral

libraryDependencies += "gov.nist.math" % "jama" % "1.0.3"

//libraryDependencies += "org.geotools" % "gt-referencing" % "9.3"