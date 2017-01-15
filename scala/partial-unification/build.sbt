scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
