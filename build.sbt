lazy val root = project
  .in(file("."))
  .settings(
    name := "http4s-altdsl",
    version := "0.1.0",
    scalaVersion := D.V.scala213,
    crossScalaVersions := Seq(D.V.scala213)
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-core"   % "0.21.4",
      "com.chuusai"   %% "shapeless"     % "2.4.0-M1",
      "eu.timepit"    %% "singleton-ops" % "0.5.0",
      "org.typelevel" %% "kittens"       % "2.1.0",
      "dev.zio"       %% "zio-test"      % "1.0.0-RC21" % Test,
      "dev.zio"       %% "zio-test-sbt"  % "1.0.0-RC21" % Test,
      compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
    )
  )
  .settings(
    scalacOptions ++= Seq("language:_"),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val D = new {
  lazy val V = new {
    val scala213 = "2.13.0"
  }
}
