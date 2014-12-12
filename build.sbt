import java.lang.System.getProperty
import java.nio.charset.StandardCharsets.UTF_8
import sbt.{Tests, File}
import sbt.Keys._
import org.sbtidea.SbtIdeaPlugin._


wartremoverWarnings ++= Seq(
  Wart.Any,
  Wart.Product,
  Wart.Serializable,
  Wart.ListOps,
  Wart.Nothing,
  Wart.Null,
  Wart.Var,
  Wart.OptionPartial,
  Wart.AsInstanceOf,
  Wart.IsInstanceOf
)

wartremoverWarnings in Compile += Wart.NonUnitStatements

wartremoverErrors ++= Seq(
  Wart.Any2StringAdd,
  Wart.EitherProjectionPartial,
  Wart.Return
)

org.scalastyle.sbt.ScalastylePlugin.Settings

organization := "com.barclays"

name := "tracking"

scalaVersion := "2.11.4"

scalaBinaryVersion := "2.11"

incOptions := incOptions.value.withNameHashing(true)

resolvers ++= Seq("releases"              at "https://oss.sonatype.org/content/repositories/releases",
                  "Typesafe Releases"     at "http://repo.typesafe.com/typesafe/maven-releases/",
                  "Scalaz Bintray Repo"   at "http://dl.bintray.com/scalaz/releases")

scalacOptions ++= Seq(
  "-target:jvm-1.7",
  "-language:postfixOps", "-language:higherKinds", "-language:implicitConversions",
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-unchecked",
  //"-Xfatal-warnings",
  "-Xlint",
  //"-Yno-adapted-args" ,      // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen"
  //"-Ywarn-value-discard"
  //"-Xfuture"
)

val libs = {
  val scalazVersion = "7.1.0"
  val graphVersion = "1.9.0"
  Seq(
    "org.scalaz"                   %% "scalaz-core"                % scalazVersion          % "compile",
    "io.argonaut"                  %% "argonaut"                   % "6.1-M4"               % "compile",
    "org.scalaz.stream"            %% "scalaz-stream"              % "0.6a"                 % "compile",
    "joda-time"                     % "joda-time"                  % "2.5"                  % "compile"
  )
}

libraryDependencies ++= libs

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.eclipseOutput := Some(".target")

cleanFiles <+= baseDirectory(_ / "logs")

compile <<= (compile in Compile) map { result =>
  require(getProperty("file.encoding") == UTF_8.toString, s"$UTF_8 required. Please set java system property file.encoding")
  result
}
