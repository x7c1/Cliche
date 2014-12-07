import sbt.Defaults.runnerInit
import sbt.Keys._
import sbt._

object SampleFrameworkPlugin extends sbt.Plugin {

  lazy val sampleSettings =
    LocationDefinition.settings ++
      SampleGeneratorDefinition.settings

  trait CommonLocations {
    def routingPath = "routing/src/main/scala"
  }

  object LocationDefinition extends CommonLocations {
    def settings = Seq(
      (unmanagedSourceDirectories in Compile) ++= SourceDirs on baseDirectory.value
    )
    private object SourceDirs {
      def on(dir: File) = Seq(
        dir / "service/src/main/scala",
        dir / routingPath )
    }
  }

  object SampleGeneratorDefinition extends CommonLocations {

    lazy val generateFoo = taskKey[Unit]("task to generate files which use AST")

    lazy val generator = Def.settingKey[String]("generator location")

    lazy val hello = TaskKey[Unit]("hello", "sample greeting")

    def settings = Seq(
      hello := {
        println(Keys.baseDirectory.value)
        println(Keys.baseDirectory.value / "routing")
        println(Keys.sourceDirectory.value)
        println(Keys.sourceManaged.value)
      },
      (generator in generateFoo) := extractRoutingName(baseDirectory.value),
      MainRunner.settingFor(generator, generateFoo)
    )
    private def extractRoutingName(dir: File) = {
      val routingDir = dir / routingPath
      val targets = routingDir ** "*Routing.scala"
      val pattern =  """^([^/]+/?.*).scala$""".r
      val toRelative = (_: File).relativeTo(routingDir).map(_.getPath)

      targets.get.headOption.flatMap(toRelative) match {
        case Some(pattern(x)) => x.replaceAll("/", ".")
        case _ => throw new IllegalArgumentException(s"routing not found")
      }
    }

  }
}

object MainRunner {

  // see also:
  // http://www.scala-sbt.org/0.13.5/docs/faq.html#how-can-i-create-a-custom-run-task-in-addition-to-run
  def settingFor(
    mainLocation: SettingKey[String], taskKey: TaskKey[Unit]): Def.Setting[Task[Unit]] = {

    taskKey <<= initScoped(taskKey.scopedKey, runnerInit)
      .zipWith((streams, fullClasspath in Compile, mainLocation in taskKey ).identityMap){

      case (a,b) => (a,b).map {
        case ((scalaRun), (taskStreams, classPath, main)) =>
          val args = Seq()
          toError(scalaRun.run(main, Attributed.data(classPath), args, taskStreams.log))
      }
    }
  }
}
