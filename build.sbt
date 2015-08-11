name := "EpSa"

version := "1.0"

val versions = Map[String, String](
  "akka"         -> "2.3.11",
  "akka-http"    -> "1.0",
  "logback"      -> "1.1.2",
  "poi"          -> "3.12",
  "scala"        -> "2.11.7",
  "suiryc-scala" -> "0.0.2-SNAPSHOT",
  "tape"         -> "1.2.3"
)

scalaVersion := versions("scala")

libraryDependencies ++= Seq(
  "ch.qos.logback"    %  "logback-classic"                   % versions("logback"),
  "com.squareup"      %  "tape"                              % versions("tape"),
  "com.typesafe.akka" %% "akka-actor"                        % versions("akka"),
  "com.typesafe.akka" %% "akka-stream-experimental"          % versions("akka-http"),
  "com.typesafe.akka" %% "akka-http-core-experimental"       % versions("akka-http"),
  "com.typesafe.akka" %% "akka-http-experimental"            % versions("akka-http"),
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % versions("akka-http"),
  "com.typesafe.akka" %% "akka-slf4j"                        % versions("akka"),
  "org.apache.poi"    %  "poi"                               % versions("poi"),
  "org.apache.poi"    %  "poi-ooxml"                         % versions("poi"),
  "suiryc"            %% "suiryc-scala-core"                 % versions("suiryc-scala"),
  "suiryc"            %% "suiryc-scala-log"                  % versions("suiryc-scala"),
  "suiryc"            %% "suiryc-scala-javafx"               % versions("suiryc-scala")
)

resolvers += Resolver.mavenLocal
