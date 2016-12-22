import java.nio.file.{Files, Paths}

import sbt.File

import scala.xml.{Node, XML}

object PomProjectLoader {

  def load(pom: File): PomProject = {
    val elem = {
      val bytes = Files readAllBytes Paths.get(pom.getAbsolutePath)
      XML loadString new String(bytes, "UTF-8")
    }
    PomProject(
      groupId = (elem \ "groupId").text,
      artifactId = (elem \ "artifactId").text,
      version = (elem \ "version").text,
      packaging = (elem \ "packaging").headOption.map(_.text),
      dependencies = elem \ "dependencies" \ "dependency" map toDependency
    )
  }

  private def toDependency(node: Node) =
    PomProjectDependency(
      groupId = (node \ "groupId").text,
      artifactId = (node \ "artifactId").text,
      version = (node \ "version").text,
      moduleType = (node \ "type").headOption.map(_.text)
    )
}
