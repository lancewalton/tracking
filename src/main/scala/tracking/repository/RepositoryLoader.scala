package tracking.repository

import java.io.{File, FilenameFilter}
import scala.io.Source
import scalaz.std.list._
import scalaz.syntax.applicative.ToApplyOpsUnapply
import scalaz.syntax.traverse.ToTraverseOps
import scalaz.syntax.validation.ToValidationOps
import argonaut.Argonaut.StringToParseWrap
import tracking.json.{metaCodec, projectStatusCodec}
import tracking.model.{Project, ProjectStatus}
import java.io.FileFilter
import scalaz.syntax.equal._
import scalaz.std.string._
import tracking.model.Meta
import scalaz.ValidationNel

object RepositoryLoader {
  def apply(directory: File): LoadedRepository[List[Project]] =
    directory
      .listFiles(new FileFilter() { def accept(file: File) = file.isDirectory }).toList
      .map(loadProject)
      .sequenceU

  private def loadProject(directory: File) = (loadMeta(directory) |@| loadProjectStatuses(directory))(Project.apply)

  private def loadMeta(directory: File): ValidationNel[RepositoryLoadError, Meta] = {
    val file = new File(directory, "meta.json")
    Source
      .fromFile(file)
      .mkString
      .decodeValidation[Meta]
      .leftMap { ProblemLoadingMetaFile(file, _) }
      .toValidationNel
  }

  private def loadProjectStatuses(directory: File) =
    directory
      .listFiles(new FilenameFilter() { def accept(directory: File, filename: String) = filename.endsWith(".json") && filename =/= "meta.json" })
      .toList
      .map(loadProjectStatus)
      .sequenceU

  private def loadProjectStatus(file: File) =
    Source
      .fromFile(file)
      .mkString
      .decodeValidation[ProjectStatus]
      .leftMap { msg => ProblemLoadingProjectStatusFile(file, msg) }
      .toValidationNel
}