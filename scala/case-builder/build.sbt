
// enable publishing the jar produced by `test:package`
publishArtifact in (Test, packageBin) := true

// enable publishing the test API jar
publishArtifact in (Test, packageDoc) := true

// enable publishing the test sources jar
publishArtifact in (Test, packageSrc) := true

lazy val commonSettings = Seq(
  organization := "nlinker",
  scalaVersion := "2.11.8",
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  // for reference
  // https://github.com/scalameta/sbt-macro-example/blob/master/build.sbt#L5-L13
  // https://bintray.com/scalameta/maven
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % "1.4.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0.148" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // macroparadise plugin doesn't work in repl and scaladoc yet
  scalacOptions in (Compile, console) := Seq(),
  sources in (Compile, doc) := Nil,

  licenses +=("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  homepage := Some(url("https://github.com/nlinker/playground-scala"))
)

lazy val meta_cb = project.in(file("."))
  .settings(commonSettings: _*)

//lazy val examples = project.in(file("examples"))
//  .settings(commonSettings: _*)
//  .dependsOn(meta_cb)

//releasePublishArtifactsAction := PgpKeys.publishSigned.value
//publishTo := {
//  val sonatype = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at sonatype + "content/repositories/snapshots")
//  else
//    Some("releases" at sonatype + "service/local/staging/deploy/maven2")
//}

pomExtra :=
  <scm>
    <url>git@github.com:nlinker/playground-scala.git</url>
    <connection>scm:git:git@github.com:nlinker/playground-scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>nlinker</id>
      <name>Nick Linker</name>
      <url>http://nlinker.github.io</url>
    </developer>
  </developers>
