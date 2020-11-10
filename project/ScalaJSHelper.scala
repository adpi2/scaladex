import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object ScalaJSHelper {
  def packageScalaJS(client: Project): Seq[Setting[_]] = Seq(
    watchSources ++= (watchSources in client).value,
    // Pick fastOpt when developing and fullOpt when publishing
    resourceGenerators in Compile += Def.task {
      val (js, map) = andSourceMap((fastOptJS in (client, Compile)).value.data)
      IO.copy(
        Seq(
          js -> target.value / js.getName,
          map -> target.value / map.getName
        )
      ).toSeq
    }.taskValue,
    mappings in (Compile, packageBin) := {
      val mappingExcludingNonOptimized =
        (mappings in (Compile, packageBin)).value.filterNot { case (f, r) =>
          f.getName.endsWith("-fastopt.js") ||
            f.getName.endsWith("js.map")
        }

      val optimized = {
        val (js, map) =
          andSourceMap((fullOptJS in (client, Compile)).value.data)
        Seq(
          js -> js.getName,
          map -> map.getName
        )
      }

      mappingExcludingNonOptimized ++ optimized
    }
  )

  private def andSourceMap(aFile: java.io.File) =
    aFile -> file(aFile.getAbsolutePath + ".map")
}
