import sbt.{File, ModuleID}
import sbt._

class ArchiveCacheFinder(cacheDirectory: File) {

  def fromDependency(dependency: PomProjectDependency): Either[FinderError, ArchiveCache] = {
    val searcher = new Searcher(ModuleID(
      organization = dependency.groupId,
      name = dependency.artifactId,
      revision = dependency.version
    ))
    val factory = dependency.moduleType match {
      case Some("aar") => searcher.toAar
      case Some("jar") | None => searcher.toJar
      case Some(x) => Left(FinderError(s"unknown dependency type: $x"))
    }
    factory.right flatMap searcher.loadPom.right.map
  }

  def search(moduleId: ModuleID): Either[FinderError, ArchiveCache] = {
    val searcher = new Searcher(moduleId)
    val factory = for {
      _ <- searcher.toAar.left
      _ <- searcher.toJar.left
    } yield {
      FinderError(s"neither .aar nor .jar found: $moduleId")
    }
    factory.right flatMap searcher.loadPom.right.map
  }

  private class Searcher(moduleId: ModuleID) {
    private val directory = cacheDirectory /
      moduleId.organization.replace(".", "/") /
      moduleId.name /
      moduleId.revision

    private val prefix = s"${moduleId.name}-${moduleId.revision}"

    private def load(name: String)(ifNotFound: File => String) =
      directory / name match {
        case x if x.exists() => Right(x)
        case x => Left(FinderError(ifNotFound(x)))
      }

    def loadPom = {
      load(s"$prefix.pom") { x => s"pom not found: $x" }
    }

    def toAar = for {
      archive <- load(s"$prefix.aar") { x => s"aar not found: $x" }.right
    } yield {
      ArchiveCache.aar(archive, _: File, moduleId)
    }

    def toJar = for {
      archive <- load(s"$prefix.jar") { x => s"jar not found: $x" }.right
    } yield {
      ArchiveCache.jar(archive, _: File, moduleId)
    }

  }

}

case class FinderError(message: String)
