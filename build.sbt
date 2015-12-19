import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"         -> "2.4.1",
  "akka-http"    -> "1.0",
  "epsa"         -> "1.0",
  "grizzled"     -> "1.0.2",
  "h2"           -> "1.4.190",
  "logback"      -> "1.1.3",
  "poi"          -> "3.13",
  "scala"        -> "2.11.7",
  "slick"        -> "3.1.0",
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

    mainClass in assembly := Some("epsa.Main"),

    libraryDependencies ++= Seq(
      "ch.qos.logback"     %  "logback-classic"                   % versions("logback"),
      "com.h2database"     %  "h2"                                % versions("h2"),
      "com.typesafe.akka"  %% "akka-actor"                        % versions("akka"),
      "com.typesafe.akka"  %% "akka-stream-experimental"          % versions("akka-http"),
      "com.typesafe.akka"  %% "akka-http-core-experimental"       % versions("akka-http"),
      "com.typesafe.akka"  %% "akka-http-experimental"            % versions("akka-http"),
      "com.typesafe.akka"  %% "akka-http-spray-json-experimental" % versions("akka-http"),
      "com.typesafe.akka"  %% "akka-slf4j"                        % versions("akka"),
      "com.typesafe.slick" %% "slick"                             % versions("slick"),
      "io.spray"           %% "spray-json"                        % versions("spray-json"),
      "org.apache.poi"     %  "poi"                               % versions("poi"),
      "org.apache.poi"     %  "poi-ooxml"                         % versions("poi"),
      "org.clapper"        %% "grizzled-slf4j"                    % versions("grizzled"),
      "suiryc"             %% "suiryc-scala-core"                 % versions("suiryc-scala"),
      "suiryc"             %% "suiryc-scala-log"                  % versions("suiryc-scala"),
      "suiryc"             %% "suiryc-scala-javafx"               % versions("suiryc-scala")
    ),

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )
