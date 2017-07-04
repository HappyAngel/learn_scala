import sbt.Keys.{libraryDependencies, name}
import sbt._

object LearnScala extends Build {
        lazy val commonProj =  Project("Common", file("Common"))
                .settings(
                    name := "Common",
                    libraryDependencies ++= Seq(
                        "com.google.code.gson" % "gson" % "2.3.1"
                    )
                )

    lazy val learn_scala_all = (project in file("."))
            .aggregate(`commonProj`)

}