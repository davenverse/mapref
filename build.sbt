import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import com.typesafe.tools.mima.core._

ThisBuild / crossScalaVersions := Seq("2.12.14", "2.13.6", "3.0.0")

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.9.0"
val catsEffectV = "3.2.1"
val munitCatsEffectV = "1.0.3"


lazy val `mapref` = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .enablePlugins(NoPublishPlugin)
  .aggregate(core.jvm, core.js)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := "mapref",
    libraryDependencies ++= Seq(
      "org.typelevel"               %%% "cats-core"                  % catsV,
      "org.typelevel"               %%% "cats-effect-kernel"         % catsEffectV,
      "org.typelevel"               %%% "cats-effect-std"            % catsEffectV % Test,
      "org.typelevel"               %%% "munit-cats-effect-3"       % munitCatsEffectV  % Test,
    ),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.mapref.MapRef.ofScalaConcurrentTrieMap"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.mapref.MapRef.inScalaConcurrentTrieMap"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.mapref.MapRefCompanionPlatform.inScalaConcurrentTrieMap"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.mapref.MapRefCompanionPlatform.ofScalaConcurrentTrieMap")
    )
  ).jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule)},
  )

lazy val site = project.in(file("site"))
  .disablePlugins(MimaPlugin)
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(DavenverseMicrositePlugin)
  .dependsOn(core.jvm)
  .settings(
    micrositeDescription := "A Reference Optimized Around Maps",
  )


