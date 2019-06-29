import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"          -> "2.5.23",
  "config"        -> "1.3.4",
  "epsa"          -> "1.1-SNAPSHOT",
  "h2"            -> "1.4.199",
  "html-cleaner"  -> "2.22",
  "httpclient"    -> "4.5.9",
  "javafx"        -> "12.0.1",
  "logback"       -> "1.2.3",
  "monix"         -> "3.0.0-RC3",
  "poi"           -> "4.1.0",
  "scala"         -> "2.12.8",
  "scala-logging" -> "3.9.2",
  "scalatest"     -> "3.0.8",
  "scopt"         -> "3.7.1",
  "slf4j"         -> "1.7.26",
  "slick"         -> "3.3.2",
  "simple-odf"    -> "0.8.2-incubating",
  "spray-json"    -> "1.3.5",
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
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:_",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:_",
      "-Ywarn-value-discard"
    ),
    resolvers += Resolver.mavenLocal,

    parallelExecution in Test := false,
    mainClass in assembly := Some("epsa.Main"),

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
      "org.apache.poi"              %  "poi-ooxml"                         % versions("poi"),
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
    mappings in (Compile, packageBin) ~= remap,

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
assembledMappings in assembly ~= { mappings =>
  mappings.map { m =>
    if (m.sourcePackage.isEmpty) m.copy(mappings = remap(m.mappings).toVector)
    else m
  }
}

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x if x.startsWith("application.conf") => MergeStrategy.discard
  case x if x.startsWith("library.properties") => MergeStrategy.discard
  case PathList("javax", "xml", _ @ _*) => MergeStrategy.first
  case x => (assemblyMergeStrategy in assembly).value.apply(x)
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
