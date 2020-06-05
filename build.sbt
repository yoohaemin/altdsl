lazy val root = project
  .in(file("."))
  .settings(
    name := "http4s-altdsl",
    version := "0.1.0",
    scalaVersion := D.V.scala213,
    crossScalaVersions := Seq(D.V.scala213, D.V.scala212
      , D.V.dotty
    )
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core" % "0.21.4",
      "com.chuusai" %% "shapeless" % "2.4.0-M1"
).map(_.withDottyCompat(scalaVersion.value))
  )
  .settings(
    scalacOptions ++= { if (isDotty.value) Seq("-language:Scala2Compat") else Nil },
    scalacOptions ++= { if (scalaVersion.value == D.V.scala212) Seq("-Ypartial-unification") else Nil}
  )

lazy val D = new {
  lazy val V = new {
    val scala213 = "2.13.1"
    val scala212 = "2.12.11"
    val dotty    = "0.24.0-RC1"
  }
}
