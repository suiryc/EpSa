import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"          -> "2.4.17",
  "epsa"          -> "1.0-SNAPSHOT",
  "h2"            -> "1.4.193",
  "html-cleaner"  -> "2.19",
  "httpclient"    -> "4.5.4",
  "logback"       -> "1.1.8",
  // Remain on POI 3.14 until 4.0 because POI 3.15 made all functions to get Cell type deprecated ...
  // (one will get un-deprecated, unless POI developers become deprecated)
  "poi"           -> "3.14",
  "scala"         -> "2.12.1",
  "scala-logging" -> "3.5.0",
  "scalatest"     -> "3.0.1",
  "slf4j"         -> "1.7.21",
  "slick"         -> "3.2.0-M2",
  "simple-odf"    -> "0.8.1-incubating",
  "spray-json"    -> "1.3.2",
  "suiryc-scala"  -> "0.0.2-SNAPSHOT"
)


lazy val epsa = project.in(file(".")).
  enablePlugins(BuildInfoPlugin, GitVersioning).
  settings(
    organization := "suiryc",
    name := "EpSa",
    // Note: if we want to let sbt-git generate the version, we need to comment
    // "version", uncomment "git.baseVersion" and remove "-SNAPSHOT" (sbt-git
    // will append it if necessary).
    version := versions("epsa"),
    //git.baseVersion := versions("epsa"),
    scalaVersion := versions("scala"),

    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      git.gitHeadCommit,
      scalaVersion,
      sbtVersion
    ),
    buildInfoPackage := "epsa",
    buildInfoObject := "Info",
    buildInfoUsePackageAsPath := true,

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      // Compiler warns of dead code after FXMLLoader.load ...
      //"-Ywarn-dead-code",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-unused",
      "-Ywarn-unused-import"
    ),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    resolvers += Resolver.mavenLocal,

    parallelExecution in Test := false,
    mainClass in assembly := Some("epsa.Main"),

    libraryDependencies ++= Seq(
      "ch.qos.logback"              %  "logback-classic"                   % versions("logback"),
      "com.h2database"              %  "h2"                                % versions("h2"),
      "com.typesafe.akka"           %% "akka-actor"                        % versions("akka"),
      "com.typesafe.akka"           %% "akka-slf4j"                        % versions("akka"),
      "com.typesafe.akka"           %% "akka-stream"                       % versions("akka"),
      "com.typesafe.scala-logging"  %% "scala-logging"                     % versions("scala-logging"),
      "com.typesafe.slick"          %% "slick"                             % versions("slick"),
      "io.spray"                    %% "spray-json"                        % versions("spray-json"),
      "net.sourceforge.htmlcleaner" %  "htmlcleaner"                       % versions("html-cleaner"),
      "org.apache.httpcomponents"   %  "httpclient"                        % versions("httpclient")
        exclude ("commons-logging", "commons-logging"),
      "org.apache.odftoolkit"       %  "simple-odf"                        % versions("simple-odf")
        exclude("commons-logging", "commons-logging")
        exclude("log4j", "log4j")
        exclude("org.slf4j", "slf4j-log4j12"),
      "org.apache.poi"              %  "poi"                               % versions("poi"),
      "org.apache.poi"              %  "poi-ooxml"                         % versions("poi"),
      "org.scalatest"               %% "scalatest"                         % versions("scalatest")    % "test",
      "org.slf4j"                   %  "jcl-over-slf4j"                    % versions("slf4j"),
      "org.slf4j"                   %  "log4j-over-slf4j"                  % versions("slf4j"),
      "org.slf4j"                   %  "slf4j-api"                         % versions("slf4j"),
      "suiryc"                      %% "suiryc-scala-core"                 % versions("suiryc-scala"),
      "suiryc"                      %% "suiryc-scala-log"                  % versions("suiryc-scala"),
      "suiryc"                      %% "suiryc-scala-javafx"               % versions("suiryc-scala")
    ),

    assemblyMergeStrategy in assembly := {
      case PathList("javax", "xml", _ @ _*) => MergeStrategy.first
      case v if v.startsWith("library.properties") => MergeStrategy.discard
      case v => MergeStrategy.defaultMergeStrategy(v)
    },

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )
