import Dependencies._

val scala213 = "2.13.16"
val scala3 = "3.3.5"

ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / tlBaseVersion := "0.1"

// publishing info
inThisBuild(
  Seq(
    organization := "lgbt.princess",
    versionScheme := Some("early-semver"),
    homepage := Some(url("https://github.com/NthPortal/lifts")),
    licenses := Seq(License.Apache2),
    developers := List(
      Developer(
        "NthPortal",
        "Marissa",
        "dev@princess.lgbt",
        url("https://github.com/NthPortal"),
      ),
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/NthPortal/lifts"),
        "scm:git:git@github.com:NthPortal/lifts.git",
        "scm:git:git@github.com:NthPortal/lifts.git",
      ),
    ),
  ),
)

// CI config
inThisBuild(
  Seq(
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches ++= Seq(
      RefPredicate.StartsWith(Ref.Tag("v")),
    ),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8")),
    githubWorkflowBuildPostamble ++= Seq(
      WorkflowStep.Sbt(
        name = Some("scalafmt"),
        commands = List("scalafmtCheckAll", "scalafmtSbtCheck"),
      ),
    ),
    githubWorkflowBuildMatrixFailFast := Some(true),
  ),
)

lazy val sharedSettings = Def.settings(
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(kindProjector)
      case _ => Nil
    }
  },
  scalacOptions ++= Seq(
    "-language:implicitConversions",
    "-feature",
    "-Werror",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-Xlint:_", "-Xsource:3")
      case _ => Nil
    }
  },
  mimaPreviousArtifacts := Set().map(organization.value %%% name.value % _),
  mimaFailOnNoPrevious := false,
)

lazy val lifts = tlCrossRootProject
  .aggregate(
    core,
    laws,
    mtl,
    ce,
  )

lazy val core =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(sharedSettings)
    .settings(
      name := "lifts",
      libraryDependencies ++= Seq(
        catsCore.value,
      ),
    )

lazy val laws =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("laws"))
    .dependsOn(core)
    .settings(sharedSettings)
    .settings(
      name := "lifts-laws",
      libraryDependencies ++= Seq(
        catsCore.value,
        catsLaws.value,
        catsTestkit.value,
        munit.value,
        munitDiscipline.value,
      ),
    )

lazy val mtl =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("mtl"))
    .dependsOn(core)
    .settings(sharedSettings)
    .settings(
      name := "lifts-mtl",
      libraryDependencies ++= Seq(
        catsCore.value,
        catsMtl.value,
        catsMtlLaws.value,
        catsEffect.value % Test,
        munitCatsEffect.value,
        munitDiscipline.value,
      )
    )

lazy val ce =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("ce"))
    .dependsOn(core, laws % "compile->test", mtl % "compile->test")
    .settings(sharedSettings)
    .settings(
      name := "lifts-ce",
      libraryDependencies ++= Seq(
        catsEffect.value,
        catsCore.value % Test,
        catsEffectTestkit.value,
        catsMtl.value % Test,
        munitCatsEffect.value,
      )
    )
