name := "A Project"

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature","-deprecation","-language:postfixOps", "-encoding", "UTF-8", "-target:jvm-1.8", "-unchecked",
  "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")
  
scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= {
	val akkaV  = "2.3.8"
  	val sprayV = "1.3.3"
  	Seq(
	"com.typesafe.akka"			%% "akka-actor" 					% akkaV 				withSources() withJavadoc,
	"com.typesafe.akka" 		%% "akka-cluster" 					% akkaV 				withSources() withJavadoc,
	"com.typesafe.akka" 		%% "akka-testkit" 					% akkaV 				withSources() withJavadoc,
	"org.scalatest" 			% "scalatest_2.11" 					% "2.2.4" 				% "test",
	"com.typesafe.akka" 		%% "akka-contrib" 					% akkaV 				withSources() withJavadoc,
	"com.typesafe.akka" 		%% "akka-persistence-experimental" 	% akkaV 				withSources() withJavadoc,
	"com.github.scullxbones" 	%% "akka-persistence-mongo-rxmongo" % "0.3.0",
	"org.reactivemongo" 		%% "reactivemongo" 					% "0.10.5.0.akka23",
	"org.json4s" 				%% "json4s-jackson" 				% "3.2.11",
	"com.wandoulabs.akka" 		%%  "spray-websocket"       		% "0.1.4"           	withSources() withJavadoc,
    "io.spray"            		%%  "spray-json"            		% "1.3.1"           	withSources() withJavadoc,
    "io.spray"            		%%  "spray-can"             		% sprayV            	withSources() withJavadoc,
    "io.spray"            		%%  "spray-routing"         		% sprayV            	withSources() withJavadoc,
    "com.typesafe.akka"   		%%  "akka-slf4j"            		% akkaV             	withSources() withJavadoc,
    "io.spray"            		%%  "spray-testkit"         		% sprayV   				% "test" withSources() withJavadoc,
    "junit"               		%   "junit"                 		% "4.12"   				% "test",
    "org.specs2"          		%%  "specs2"                		% "2.4.17" 				% "test",
    "ch.qos.logback"      		%   "logback-classic"       		% "1.1.3"
    )
}