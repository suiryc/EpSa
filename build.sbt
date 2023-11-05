import sbt.*
import Keys.*
import suiryc.scala.sbt.{AssemblyEx, Versioning}

lazy val versions = Map[String, String](
  "akka"          -> "2.6.21",
  "config"        -> "1.4.3",
  "h2"            -> "1.4.200",
  "html-cleaner"  -> "2.29",
  "httpclient"    -> "4.5.14",
  "javafx"        -> "12.0.1",
  // log4j2 extra dependencies for slf4j, if needed
  "log4j2"        -> "2.20.0",
  "logback"       -> "1.4.11",
  "monix"         -> "3.4.0",
  "poi"           -> "5.2.4",
  "scala"         -> "2.13.12",
  "scala-logging" -> "3.9.5",
  "scalatest"     -> "3.2.17",
  "scopt"         -> "4.1.0",
  "slf4j"         -> "2.0.9",
  "slick"         -> "3.4.1",
  // Notes:
  // simple-odf was part of odftoolkit when handled in apache incubator. Latest
  // official release is 0.6.2-incubating, while maven has a 0.8.2-incubating.
  // Deprecated maven: https://search.maven.org/artifact/org.apache.odftoolkit/simple-odf
  // Once moved out of apache, 'simple' subproject disappeared in refactoring:
  //  - merge: https://github.com/tdf/odftoolkit/commit/2a6d4fc2b45eed57f83604c5de92941629508ece
  //  - commit: https://github.com/svanteschubert/apache-odftoolkit/commit/3fee8cf400f006952319449be8794a7104515abd
  // Latest maven (after move): https://search.maven.org/artifact/org.odftoolkit/simple-odf
  "simple-odf"    -> "0.8.2-incubating",
  "spray-json"    -> "1.3.6",
  "suiryc-scala"  -> "0.0.4-SNAPSHOT"
)

// Determine version (and log it) once before using it.
lazy val projectVersion = Versioning.version
Global / onLoad := {
  val original = (Global / onLoad).value
  sLog.value.info(s"Project version: $projectVersion")
  original
}

lazy val epsa = project.in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    organization := "suiryc",
    name := "EpSa",
    version := projectVersion,
    scalaVersion := versions("scala"),

    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      BuildInfoKey.action("gitHeadCommit") {
        Versioning.commitId
      },
      scalaVersion,
      sbtVersion,
      BuildInfoKey.action("buildTime") {
        System.currentTimeMillis
      }
    ),
    buildInfoPackage := "epsa",
    buildInfoObject := "Info",
    buildInfoUsePackageAsPath := true,

    scalacOptions ++= Seq(
      "-explaintypes",
      "-feature",
      "-unchecked",
      "-Werror",
      "-Wdead-code",
      "-Wextra-implicit",
      "-Wnumeric-widen",
      "-Wunused",
      "-Wvalue-discard",
      "-Xcheckinit",
      "-Xlint"
    ),
    resolvers += Resolver.mavenLocal,

    Test / parallelExecution := false,
    assembly / mainClass := Some("epsa.Main"),

    libraryDependencies ++= Seq(
      "ch.qos.logback"              %  "logback-classic"                   % versions("logback"),
      "com.github.scopt"            %% "scopt"                             % versions("scopt"),
      "com.h2database"              %  "h2"                                % versions("h2"),
      "com.typesafe"                %  "config"                            % versions("config"),
      "com.typesafe.akka"           %% "akka-actor"                        % versions("akka"),
      "com.typesafe.akka"           %% "akka-slf4j"                        % versions("akka"),
      "com.typesafe.akka"           %% "akka-stream"                       % versions("akka"),
      "com.typesafe.scala-logging"  %% "scala-logging"                     % versions("scala-logging"),
      "com.typesafe.slick"          %% "slick"                             % versions("slick"),
      "io.monix"                    %% "monix"                             % versions("monix"),
      "io.spray"                    %% "spray-json"                        % versions("spray-json"),
      "net.sourceforge.htmlcleaner" %  "htmlcleaner"                       % versions("html-cleaner"),
      "org.apache.httpcomponents"   %  "httpclient"                        % versions("httpclient")
        exclude ("commons-logging", "commons-logging"),
      "org.apache.logging.log4j"    %  "log4j-api"                         % versions("log4j2"),
      "org.apache.logging.log4j"    %  "log4j-to-slf4j"                    % versions("log4j2"),
      "org.apache.odftoolkit"       %  "simple-odf"                        % versions("simple-odf")
        exclude("commons-logging", "commons-logging")
        exclude("log4j", "log4j")
        exclude("org.slf4j", "slf4j-log4j12"),
      "org.apache.poi"              %  "poi"                               % versions("poi"),
      // poi v5.0.0 comes with batik-all which depends on all batik-xxx submodules
      // while also providing the same .class files but with different content ...
      "org.apache.poi"              %  "poi-ooxml"                         % versions("poi")
        exclude("org.apache.xmlgraphics", "batik-all"),
      "org.openjfx"                 %  "javafx-base"                       % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                 %  "javafx-controls"                   % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                 %  "javafx-fxml"                       % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                 %  "javafx-graphics"                   % versions("javafx") classifier jfxPlatform,
      "org.scalatest"               %% "scalatest"                         % versions("scalatest")    % "test",
      "org.slf4j"                   %  "jcl-over-slf4j"                    % versions("slf4j"),
      "org.slf4j"                   %  "log4j-over-slf4j"                  % versions("slf4j"),
      "org.slf4j"                   %  "slf4j-api"                         % versions("slf4j"),
      "suiryc"                      %% "suiryc-scala-core"                 % versions("suiryc-scala"),
      "suiryc"                      %% "suiryc-scala-log"                  % versions("suiryc-scala"),
      "suiryc"                      %% "suiryc-scala-javafx"               % versions("suiryc-scala")
    ),

    // Replace mappings for jar generation
    Compile / packageBin / mappings ~= remap,

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )

// Files to exclude: are generated inside 'target/classes' if running
// from IDE. Useful when not cleaning up before packaging ...
// Note: "application.conf*" is also discarded in assembly merge strategy.
lazy val excludedFiles = Set("application.conf", "application.conf.bak")

def remap(mappings: Seq[(File, String)]): Seq[(File, String)] = {
  // The 'package' path
  val matchPath = "package"
  // Get all files to package, and determine the actual destination path
  val toPackage = mappings.filter {
    case (_, dst) => (dst != matchPath) && Path(dst).asPath.startsWith(matchPath)
  }.map {
    case (src, dst) =>
      val dstPath = Path(dst).asPath
      src -> dstPath.getParent.resolveSibling(dstPath.getFileName).toString
  }
  val toPackageSrc = toPackage.map(_._1).toSet
  val toPackageDst = toPackage.map(_._2).toSet
  // Replace mappings that we are explicitly packaging
  mappings.filter {
    case (src, dst) => !toPackageSrc.contains(src) && !toPackageDst.contains(dst) && !excludedFiles.contains(dst)
  } ++ toPackage
}

// Since sbt-assembly 1.0.0, running tests must be configured explicitly
assembly / test := (Test / test).value

ThisBuild / assemblyMergeStrategy := {
  case PathList(x @ _*) if x.last == "module-info.class" => MergeStrategy.discard
  case x if x.startsWith("library.properties") => MergeStrategy.discard
  case "application.conf" => AssemblyEx.strategies.concatLibsThenProject
  case x if excludedFiles.contains(x) => AssemblyEx.strategies.libsOnly
  // Note: monix 3.4.0 (monix-internal-jctools) embeds a few package classes
  // that comes from scala-collection-compat.
  case PathList("scala", "collection", "compat", "package$.class") => MergeStrategy.first
  case PathList("scala", "collection", "compat", "immutable", "package$.class") => MergeStrategy.first
  case PathList("scala", "util", "control", "compat", "package$.class") => MergeStrategy.first
  case "scala-collection-compat.properties" => MergeStrategy.first
  case x => (ThisBuild / assemblyMergeStrategy).value.apply(x)
}

lazy val jfxPlatform = {
  val osName = System.getProperty("os.name", "").toLowerCase
  if (osName.startsWith("mac")) "mac"
  else if (osName.startsWith("win")) "win"
  else "linux"
}

lazy val install = taskKey[Unit]("Installs application")
install := {
  import suiryc.scala.io.RichFile
  import suiryc.scala.sys.OS

  val jar = assembly.value
  val targetFolder = if (OS.isLinux) {
    Path(RichFile.userHome) / "progs" / "EpSa"
  } else {
    Path("C:\\") / "Progs" / "EpSa"
  }
  sLog.value.info(s"Copying files to: $targetFolder")
  List(jar).foreach { src =>
    val targetPath = targetFolder / src.getName
    IO.copyFile(src, targetPath.asFile)
  }
}
