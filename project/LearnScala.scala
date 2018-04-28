import sbt.Keys.{libraryDependencies, name}
import sbt._

object LearnScala extends Build {
        lazy val commonProj =  Project("Common", file("Common"))
                .settings(
                    name := "Common",
                    libraryDependencies ++= Seq(
                        "com.google.code.gson" % "gson" % "2.3.1",
                        "commons-codec" % "commons-codec" % "1.10",
                        "com.typesafe.play" %% "play-json" % "2.5.4",
                        "net.sf.json-lib" % "json-lib" % "2.2.3",
                        "com.google.guava" % "guava" % "20.0",
                        "com.xiaomi" %% "ai-api-common" % "1.1.37",
                        "com.xiaomi" % "ai-api-spec_2.11" % "1.0.27",
                        "ai.x" %% "play-json-extensions" % "0.10.0"

                    )
                )

    lazy val learn_scala_all = (project in file("."))
            .aggregate(`commonProj`)

}