import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Dependencies {
  private object V {
    val cats = "2.13.0"
    val catsEffect = "3.6.0"
    val catsMtl = "1.5.0"
  }

  val catsCore = Def.setting("org.typelevel" %%% "cats-core" % V.cats)
  val catsEffect = Def.setting("org.typelevel" %%% "cats-effect-kernel" % V.catsEffect)
  val catsEffectTestkit =
    Def.setting("org.typelevel" %%% "cats-effect-kernel-testkit" % V.catsEffect % Test)
  val catsLaws = Def.setting("org.typelevel" %%% "cats-laws" % V.cats)
  val catsMtl = Def.setting("org.typelevel" %%% "cats-mtl" % V.catsMtl)
  val catsMtlLaws = Def.setting("org.typelevel" %%% "cats-mtl-laws" % V.catsMtl)
  val catsTestkit = Def.setting("org.typelevel" %%% "cats-testkit" % V.cats % Test)
  val munit = Def.setting("org.scalameta" %%% "munit" % "1.0.0" % Test)
  val munitDiscipline = Def.setting("org.typelevel" %%% "discipline-munit" % "2.0.0" % Test)

  val kindProjector = compilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full
  )
}
