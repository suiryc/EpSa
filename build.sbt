name := "EpSa"

version := "1.0"

val versions = Map[Symbol, String](
  'akka      -> "2.3.11",
  'akkaHttp  -> "1.0-RC4",
  'logback   -> "1.1.2",
  'poi       -> "3.12",
  'scala     -> "2.11.7"
)

scalaVersion := versions('scala)

libraryDependencies ++= Seq(
  "ch.qos.logback"    %  "logback-classic"                   % versions('logback),
  "com.typesafe.akka" %% "akka-actor"                        % versions('akka),
  "com.typesafe.akka" %% "akka-stream-experimental"          % versions('akkaHttp),
  "com.typesafe.akka" %% "akka-http-core-experimental"       % versions('akkaHttp),
  "com.typesafe.akka" %% "akka-http-experimental"            % versions('akkaHttp),
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % versions('akkaHttp),
  "com.typesafe.akka" %% "akka-slf4j"                        % versions('akka),
  "org.apache.poi"    %  "poi"                               % versions('poi),
  "org.apache.poi"    %  "poi-ooxml"                         % versions('poi)
)
