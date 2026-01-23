import coursierapi.*
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler
import software.amazon.smithy.model.loader.ModelDiscovery
import software.amazon.smithy.model.loader.ModelManifestException

import java.io.File
import java.net.URI
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.FileSystems
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.chaining.*
import software.amazon.smithy.model.validation.ValidatedResult

object ModelLoader {

  private def makeClassLoaderForJars(
    jars: List[File]
  ): URLClassLoader =
    new URLClassLoader(
      jars.map(_.toURI().toURL()).toArray,
      this.getClass().getClassLoader(),
    )

  def load(
    specs: Set[File],
    jars: List[File],
  ): ValidatedResult[Model] = Model
    .assembler()
    .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)
    .pipe(addJarModels(jars))
    .pipe(addFileImports(specs))
    .assemble()

  private def addJarModels(
    jars: List[File]
  ): ModelAssembler => ModelAssembler = { m =>
    jars
      .flatMap(loadModelsFromJar)
      .foreach(
        m.addImport(_)
      )

    m
  }

  private def loadModelsFromJar(
    file: File
  ): List[URL] = ModelDiscovery.findModels(file.toURI().toURL()).asScala.toList

  private def addFileImports(
    imports: Iterable[File]
  ): ModelAssembler => ModelAssembler = { assembler =>
    imports.foreach(f => assembler.addImport(f.toPath()))
    assembler
  }

  def resolveDependencies(
    dependencies: List[String],
    repositories: List[String],
  ): List[File] = {
    val creds = coursierCredentialsByHost()

    val repos = repositories.map(MavenRepository.of).map(addCredsToRepo(_, creds))

    val deps = dependencies
      .map(Dependency.parse(_, ScalaVersion.of("3")))

    Fetch
      .create
      .addRepositories(repos*)
      .addDependencies(deps*)
      .fetch()
      .asScala
      .toList
  }

  private def coursierCredentialsByHost(): Map[String, Credentials] =
    sys
      .env
      .get("COURSIER_CREDENTIALS")
      .flatMap {
        case s"$host($_) $u:$p" => Some(host -> coursierapi.Credentials.of(u, p))
        // untested
        case s"$host $u:$p" => Some(host -> coursierapi.Credentials.of(u, p))
        case _              => None
      }
      .toMap

  private def addCredsToRepo(repo: MavenRepository, credsByHost: Map[String, Credentials])
    : MavenRepository = {
    val host = URI.create(repo.getBase()).getHost()
    credsByHost.get(host).foldLeft(repo)(_.withCredentials(_))
  }

}
