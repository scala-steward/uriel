val appVersion:String = "0.1"
val globalScalaVersion = "3.3.6"

ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion

ThisBuild / tlBaseVersion := appVersion
ThisBuild / tlCiReleaseBranches := Seq()

ThisBuild / nativeConfig ~= {
  _.withLTO(scala.scalanative.build.LTO.thin)
   .withMode(scala.scalanative.build.Mode.releaseFast)
   .withGC(scala.scalanative.build.GC.commix)
}

lazy val bitfrost = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "bitfrost",
    version := "0.0.03",
    Compile / mainClass := Some("ai.dragonfly.bitfrost.verification.ConversionFidelity"),
    libraryDependencies ++= Seq(
      "ai.dragonfly" %%% "mesh" % "0.14",
      "ai.dragonfly" %%% "spatial" % "0.1"
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings()

lazy val demo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .dependsOn(bitfrost)
  .settings(
    name := "demo",
    Compile / mainClass := Some("Demo"),
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings()


lazy val root = tlCrossRootProject.aggregate(bitfrost, tests).settings(name := "bitfrost")

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin).settings(
  mdocVariables := Map(
    "VERSION" -> appVersion,
    "SCALA_VERSION" -> globalScalaVersion
  ),
  laikaConfig ~= { _.withRawContent }
)

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin) // also enables the ScalaUnidocPlugin
  .settings(
    name := "bitfrost-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        bitfrost.jvm,
        bitfrost.js,
        bitfrost.native
      )
  )

lazy val tests = crossProject(
    JVMPlatform,
    JSPlatform,
    NativePlatform
  )
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(bitfrost)
  .settings(
    name := "bitfrost-tests",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test
  )