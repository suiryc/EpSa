import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"          -> "2.6.15",
  "config"        -> "1.4.1",
  "epsa"          -> "1.1-SNAPSHOT",
  "h2"            -> "1.4.200",
  "html-cleaner"  -> "2.24",
  "httpclient"    -> "4.5.13",
  "javafx"        -> "12.0.1",
  "logback"       -> "1.2.3",
  "monix"         -> "3.4.0",
  "poi"           -> "5.0.0",
  "scala"         -> "2.13.6",
  "scala-logging" -> "3.9.3",
  "scalatest"     -> "3.2.9",
  "scopt"         -> "4.0.1",
  "slf4j"         -> "1.7.31",
  "slick"         -> "3.3.3",
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

def remap(mappings: Seq[(File, String)]): Seq[(File, String)] = {
  // Files to exclude: are generated inside 'target/classes' if running
  // from IDE. Useful when not cleaning up before packaging ...
  // Note: "application.conf*" is also discarded in assembly merge strategy.
  val exclude = Set("application.conf", "application.conf.bak")
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
    case (src, dst) => !toPackageSrc.contains(src) && !toPackageDst.contains(dst) && !exclude.contains(dst)
  } ++ toPackage
}

// Replace mappings for fat jar generation
assembly / assembledMappings ~= { mappings =>
  mappings.map { m =>
    if (m.sourcePackage.isEmpty) m.copy(mappings = remap(m.mappings).toVector)
    else m
  }
}

ThisBuild / assemblyMergeStrategy := {
  case PathList(x @ _*) if x.last == "module-info.class" => MergeStrategy.discard
  case x if x.startsWith("application.conf") => MergeStrategy.discard
  case x if x.startsWith("library.properties") => MergeStrategy.discard
  // Note: monix 3.4.0 (monix-internal-jctools) embeds a few package classes
  // that comes from scala-collection-compat.
  case PathList("scala", "collection", "compat", "immutable", "package$.class") => MergeStrategy.first
  case PathList("scala", "collection", "compat", "immutable", "package.class") => MergeStrategy.first
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
