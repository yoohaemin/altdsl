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
      "org.http4s"  %% "http4s-core"      % "0.21.4",
      "com.chuusai" %% "shapeless"        % "2.4.0-M1",
      "eu.timepit"  %% "singleton-ops"    % "0.5.0",
      "ru.tinkoff"  %% "tofu-optics-core" % "0.7.7",
      compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
    )
  )
  .settings(
    scalacOptions ++= Seq(
    )
  )

lazy val D = new {
  lazy val V = new {
    val scala213 = "2.13.1"
    val scala212 = "2.12.11"
  }
}
