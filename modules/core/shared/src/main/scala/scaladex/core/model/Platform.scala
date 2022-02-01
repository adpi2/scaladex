package scaladex.core.model

import fastparse.NoWhitespace._
import fastparse._
import scaladex.core.util.Parsers

sealed trait Platform {
  def scalaVersion: Option[ScalaVersion]
  def version: Platform.Version
  def render: String
  def encode: String
  def isValid: Boolean

  def isJava: Boolean = this match {
    case Platform.Java => true
    case _             => false
  }
  def isSbt: Boolean = this match {
    case _: Platform.SbtPlugin => true
    case _                     => false
  }
}

object Platform extends Parsers {
  sealed trait Version {
    def encode: String
  }

  object Version {
    implicit val ordering: Ordering[Version] = Ordering.by {
      case Jvm             => (true, None, None, None)
      case Js(version)     => (false, Some(version), None, None)
      case Native(version) => (false, None, Some(version), None)
      case Sbt(version)    => (false, None, None, Some(version))
    }

    case object Jvm extends Version {
      override def toString: String = "JVM"
      override def encode: String = "jvm"
    }

    case class Js(version: BinaryVersion) extends Version {
      override def toString: String = s"Js $version"
      override def encode: String = s"sjs${version.encode}"
    }

    case class Native(version: BinaryVersion) extends Version {
      override def toString: String = s"Native $version"
      override def encode: String = s"native${version.encode}"
    }

    case class Sbt(version: BinaryVersion) extends Version {
      override def toString: String = s"sbt $version"
      override def encode: String = s"sbt${version.encode}"
    }

    def parse(input: String): Option[Version] =
      input match {
        case "jvm"             => Some(Jvm)
        case s"sjs$version"    => BinaryVersion.parse(version).map(Js.apply)
        case s"native$version" => BinaryVersion.parse(version).map(Native.apply)
        case s"sbt$version"    => BinaryVersion.parse(version).map(Sbt.apply)
      }
  }

  // Scala > Js > Native > Sbt > Java
  implicit val ordering: Ordering[Platform] =
    Ordering.by(platform => (platform.version, platform.scalaVersion))

  case class SbtPlugin(scalaV: ScalaVersion, sbtV: BinaryVersion) extends Platform {
    override def scalaVersion: Option[ScalaVersion] = Some(scalaV)
    override def version: Version = Version.Sbt(sbtV)
    override def render: String = s"sbt $sbtV ($scalaV)"
    override def encode: String = s"_${scalaV}_${sbtV.encode}"
    override def isValid: Boolean = scalaV.isValid && SbtPlugin.isValid(sbtV)
  }

  object SbtPlugin {
    val `0.13`: BinaryVersion = MinorBinary(0, 13)
    val `1.0`: BinaryVersion = MinorBinary(1, 0)
    val stableBinaryVersions: Set[BinaryVersion] = Set(`0.13`, `1.0`)
    def isValid(version: BinaryVersion): Boolean = stableBinaryVersions.contains(version)
  }

  case object Java extends Platform {
    override def scalaVersion: Option[ScalaVersion] = None
    override def version: Version = Platform.Version.Jvm
    override def render: String = "Java"
    override def encode: String = ""
    override def isValid: Boolean = true
  }

  case class ScalaNative(
      scalaV: ScalaVersion,
      scalaNativeV: BinaryVersion
  ) extends Platform {
    override def scalaVersion: Option[ScalaVersion] = Some(scalaV)
    override def version: Version = Version.Native(scalaNativeV)
    override def render: String = s"scala-native $scalaNativeV ($scalaV)"
    override def encode: String = s"_native${scalaNativeV.encode}_${scalaV.encode}"
    override def isValid: Boolean = scalaV.isValid && ScalaNative.isValid(scalaNativeV)
  }

  object ScalaNative {
    val `0.3`: BinaryVersion = MinorBinary(0, 3)
    val `0.4`: BinaryVersion = MinorBinary(0, 4)
    val `0.4_2.13`: ScalaNative = ScalaNative(ScalaVersion.`2.13`, `0.4`)
    val `0.3_2.13`: ScalaNative = ScalaNative(ScalaVersion.`2.13`, `0.3`)
    private val stableBinaryVersions: Set[BinaryVersion] = Set(`0.3`, `0.4`)
    def isValid(version: BinaryVersion): Boolean =
      stableBinaryVersions.contains(version)
  }

  case class ScalaJs(scalaV: ScalaVersion, scalaJsV: BinaryVersion) extends Platform {
    override def scalaVersion: Option[ScalaVersion] = Some(scalaV)

    override def version: Version = Version.Js(scalaJsV)
    override def render: String = s"scala-js $scalaJsV ($scalaV)"
    override def encode: String = s"_sjs${scalaJsV.encode}_${scalaV.encode}"
    override def isValid: Boolean =
      scalaV.isValid && ScalaJs.isValid(scalaJsV)
  }

  object ScalaJs {
    val `0.6`: BinaryVersion = MinorBinary(0, 6)
    val `1.x`: BinaryVersion = MajorBinary(1)
    val `0.6_2.13`: ScalaJs = ScalaJs(ScalaVersion.`2.13`, `0.6`)
    val `1_3`: ScalaJs = ScalaJs(ScalaVersion.`3`, `1.x`)

    private val stableBinaryVersions: Set[BinaryVersion] = Set(`0.6`, `1.x`)
    def isValid(version: BinaryVersion): Boolean = stableBinaryVersions.contains(version)
  }

  case class ScalaJvm(scalaV: ScalaVersion) extends Platform {
    override def scalaVersion: Option[ScalaVersion] = Some(scalaV)
    override def version: Version = Version.Jvm
    override def render: String = scalaV.render
    override def encode: String = s"_${scalaV.encode}"
    override def isValid: Boolean = scalaVersion.exists(_.isValid)
  }

  object ScalaJvm {
    val `3`: ScalaJvm = ScalaJvm(ScalaVersion.`3`)
    val `2.13` = ScalaJvm(ScalaVersion.`2.13`)

    def fromFullVersion(fullVersion: SemanticVersion): Option[ScalaJvm] = {
      val binaryVersion = fullVersion match {
        case SemanticVersion(major, Some(minor), _, None, None, None) if major == 2 =>
          Some(MinorBinary(major, minor))
        case SemanticVersion(major, _, _, None, None, None) if major == 3 =>
          Some(MajorBinary(major))
        case _ => None
      }

      binaryVersion
        .filter(ScalaVersion.isValid)
        .map(v => ScalaJvm(ScalaVersion(v)))
    }
  }

  def parse(code: String): Option[Platform] =
    tryParse(code, x => FullParser(x))

  def FullParser[_: P]: P[Platform] =
    Parser ~ End

  def IntermediateParser[_: P]: P[(String, Option[BinaryVersion], Option[BinaryVersion])] =
    ("_sjs" | "_native" | "_" | "").! ~ (BinaryVersion.Parser.?) ~ ("_" ~ BinaryVersion.Parser).?
  def Parser[_: P]: P[Platform] =
    IntermediateParser
      .map {
        case ("_sjs", Some(jsV), Some(scalaV))        => ScalaVersion.from(scalaV).map(sv => ScalaJs(sv, jsV))
        case ("_native", Some(nativeV), Some(scalaV)) => ScalaVersion.from(scalaV).map(sv => ScalaNative(sv, nativeV))
        case ("_", Some(scalaV), Some(sbtV))          => ScalaVersion.from(scalaV).map(sv => SbtPlugin(sv, sbtV))
        case ("_", Some(scalaV), None)                => ScalaVersion.from(scalaV).map(sv => ScalaJvm(sv))
        case ("", None, None)                         => Some(Java)
        case _                                        => None
      }
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.isValid)

}
