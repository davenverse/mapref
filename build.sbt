import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.3" // current series x.y

ThisBuild / organization := "io.chrisdavenport"
ThisBuild / organizationName := "Christopher Davenport"
ThisBuild / startYear := Some(2021)
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  tlGitHubDev("christopherdavenport", "Christopher Davenport")
)

// sbt-davenverse published a snapshot from main on every push. Dropped: the
// Central Portal will not enable snapshots for the io.chrisdavenport namespace.
ThisBuild / tlCiReleaseBranches := Seq()

val Scala213 = "2.13.18"
ThisBuild / crossScalaVersions := Seq("2.12.20", Scala213, "3.3.8")
ThisBuild / scalaVersion := Scala213

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.6.1"
val catsEffectV = "3.2.1"
val munitCatsEffectV = "1.0.3"

lazy val `mapref` = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := "mapref",
    // sbt-davenverse injected these globally; sbt-typelevel-ci-release does not
    // (only sbt-typelevel-settings would), so they are restored explicitly.
    //   kind-projector / -Ykind-projector : MapRef[F, K, *]
    //   -Ypartial-unification             : Ref#imap on 2.12
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % "0.13.4" cross CrossVersion.full),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
        )
      case _ => Nil
    }),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq("-Ykind-projector")
      case Some((2, 12)) => Seq("-Ypartial-unification")
      case _ => Nil
    }),
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
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(
    laikaTheme := tlSiteHelium.value.site
      .topNavigationBar(
        homeLink = laika.helium.config.IconLink.internal(laika.ast.Path.Root / "index.md", laika.helium.config.HeliumIcon.home)
      )
      .build
  )
