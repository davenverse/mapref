import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val `mapref` = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(skip in publish := true)
  .aggregate(core)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "mapref"
  )

lazy val site = project.in(file("site"))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(skip in publish := true)
  .dependsOn(core)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(MdocPlugin)
  .settings{
    import microsites._
    Seq(
      micrositeName := "mapref",
      micrositeDescription := "A Reference Optimized Around Maps",
      micrositeAuthor := "Christopher Davenport",
      micrositeGithubOwner := "ChristopherDavenport",
      micrositeGithubRepo := "mapref",
      micrositeBaseUrl := "/mapref",
      micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.chrisdavenport/mapref_2.12",
      micrositeGitterChannelUrl := "ChristopherDavenport/libraries", // Feel Free to Set To Something Else
      micrositeFooterText := None,
      micrositeHighlightTheme := "atom-one-light",
      micrositePalette := Map(
        "brand-primary" -> "#3e5b95",
        "brand-secondary" -> "#294066",
        "brand-tertiary" -> "#2d5799",
        "gray-dark" -> "#49494B",
        "gray" -> "#7B7B7E",
        "gray-light" -> "#E5E5E6",
        "gray-lighter" -> "#F4F3F4",
        "white-color" -> "#FFFFFF"
      ),
      micrositePushSiteWith := GitHub4s,
      micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
      micrositeExtraMdFiles := Map(
          file("CHANGELOG.md")        -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "100")),
          file("CODE_OF_CONDUCT.md")  -> ExtraMdFileConfig("code-of-conduct.md",   "page", Map("title" -> "code of conduct",   "section" -> "code of conduct",   "position" -> "101")),
          file("LICENSE")             -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "102"))
      )
    )
  }

val catsV = "2.4.0"
val catsEffectV = "3.0.0-RC1"
val catsEffectTestV = "1.0.0-M1"

val specs2V = "4.8.1"

val kindProjectorV = "0.10.3"
val betterMonadicForV = "0.3.1"

// General Settings
lazy val commonSettings = Seq(
  scalaVersion := "2.13.4",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.10"),

  addCompilerPlugin("org.typelevel" % "kind-projector" % kindProjectorV cross CrossVersion.binary),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % betterMonadicForV),
  libraryDependencies ++= Seq(
    "org.typelevel"               %% "cats-core"                  % catsV,
    "org.typelevel"               %% "cats-effect"                % catsEffectV,

    "com.codecommit" %% "cats-effect-testing-specs2"              % catsEffectTestV % Test,
    "org.specs2"                  %% "specs2-core"                % specs2V       % Test,
    "org.specs2"                  %% "specs2-scalacheck"          % specs2V       % Test
  )
)

inThisBuild(List(
  organization := "io.chrisdavenport",
  homepage := Some(url("https://github.com/ChristopherDavenport/mapref")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "ChristopherDavenport",
      "Christopher Davenport",
      "chris@christopherdavenport.tech",
      url("https://github.com/ChristopherDavenport")
    )
  ),
  scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/ChristopherDavenport/mapref/blob/v" + version.value + "€{FILE_PATH}.scala"
  ),
))