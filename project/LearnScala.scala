import sbt.Keys.{libraryDependencies, name}
import sbt.{Build, _}

object LearnScala extends Build {
        lazy val commonProj =  Project("Common", file("Common"))
                .settings(
                    name := "Common",
                    libraryDependencies ++= Seq(
                        "com.google.code.gson" % "gson" % "2.3.1",
                        "commons-codec" % "commons-codec" % "1.10",
                        "net.sf.json-lib" % "json-lib" % "2.2.3",
                        "com.google.guava" % "guava" % "20.0",
                        "ai.x" %% "play-json-extensions" % "0.10.0"

                    )
                )

        lazy val fpisProj = Project("Common", file("Common"))

    lazy val learn_scala_all = (project in file("."))
            .aggregate(`commonProj`)
            .aggregate(`fpisProj`)

}