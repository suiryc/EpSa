import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"         -> "2.4.2",
  "epsa"         -> "1.0-SNAPSHOT",
  "grizzled"     -> "1.0.2",
  "h2"           -> "1.4.191",
  "logback"      -> "1.1.7",
  "poi"          -> "3.13",
  "scala"        -> "2.11.8",
  "scalatest"    -> "2.2.6",
  "slf4j"        -> "1.7.20",
  "slick"        -> "3.1.1",
  "simple-odf"   -> "0.8.1-incubating",
  "spray-json"   -> "1.3.2",
  "suiryc-scala" -> "0.0.2-SNAPSHOT"
)


lazy val epsa = project.in(file(".")).
  settings(
    organization := "suiryc",
    name := "EpSa",
    version := versions("epsa"),
    scalaVersion := versions("scala"),

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-optimize",
      "-unchecked",
      "-Yinline-warnings"
    ),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    resolvers += Resolver.mavenLocal,

    parallelExecution in Test := false,
    mainClass in assembly := Some("epsa.Main"),

    libraryDependencies ++= Seq(
      "ch.qos.logback"        %  "logback-classic"                   % versions("logback"),
      "com.h2database"        %  "h2"                                % versions("h2"),
      "com.typesafe.akka"     %% "akka-actor"                        % versions("akka"),
      "com.typesafe.akka"     %% "akka-slf4j"                        % versions("akka"),
      "com.typesafe.slick"    %% "slick"                             % versions("slick"),
      "io.spray"              %% "spray-json"                        % versions("spray-json"),
      "org.apache.odftoolkit" %  "simple-odf"                        % versions("simple-odf")
        exclude("commons-logging", "commons-logging")
        exclude("log4j", "log4j")
        exclude("org.slf4j", "slf4j-log4j12"),
      "org.apache.poi"        %  "poi"                               % versions("poi"),
      "org.apache.poi"        %  "poi-ooxml"                         % versions("poi"),
      "org.clapper"           %% "grizzled-slf4j"                    % versions("grizzled"),
      "org.scalatest"         %% "scalatest"                         % versions("scalatest")    % "test",
      "org.slf4j"             %  "jcl-over-slf4j"                    % versions("slf4j"),
      "org.slf4j"             %  "log4j-over-slf4j"                  % versions("slf4j"),
      "org.slf4j"             %  "slf4j-api"                         % versions("slf4j"),
      "suiryc"                %% "suiryc-scala-core"                 % versions("suiryc-scala"),
      "suiryc"                %% "suiryc-scala-log"                  % versions("suiryc-scala"),
      "suiryc"                %% "suiryc-scala-javafx"               % versions("suiryc-scala")
    ),

    assemblyMergeStrategy in assembly := {
      case PathList("javax", "xml", xs @ _*) => MergeStrategy.first
      case v if v.startsWith("library.properties") => MergeStrategy.discard
      case v => MergeStrategy.defaultMergeStrategy(v)
    },

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )
