package tracking.repository

import java.io.{File, FilenameFilter}

import scala.io.Source

import scalaz.std.list._
import scalaz.syntax.applicative.ToApplyOpsUnapply
import scalaz.syntax.traverse.ToTraverseOps
import scalaz.syntax.validation.ToValidationOps

import argonaut.Argonaut.StringToParseWrap
import tracking.json.projectStatusCodec
import tracking.model.{IdentifierAndTitle, Project, ProjectStatus}

object RepositoryLoader {
  def apply(directory: File): LoadedRepository[List[Project]] =
    directory
      .listFiles.toList
      .map(loadProject(_))
      .sequenceU

  private def loadProject(directory: File) =
    (extractNameAndId(directory) |@| loadProjectStatuses(directory))((nid, ss) => Project(nid, ss))

  private def extractNameAndId(directory: File) =
    directory.getName.split('.').toList match {
      case name :: Nil if (!name.isEmpty)       => IdentifierAndTitle(name, name).successNel[RepositoryLoadError]
      case name :: id :: Nil if (!name.isEmpty && !id.isEmpty) => IdentifierAndTitle(id, name).successNel[RepositoryLoadError]
      case _                 => ProjectDirectoryNameParseFailure(directory).failureNel[IdentifierAndTitle]
    }

  private def loadProjectStatuses(directory: File) =
    directory
      .listFiles(new FilenameFilter() { def accept(directory: File, filename: String) = filename.endsWith(".json") })
      .toList
      .map { loadProjectStatus(_) }
      .sequenceU

  private def loadProjectStatus(file: File) =
    Source
      .fromFile(file.getAbsolutePath)
      .mkString
      .decodeValidation[ProjectStatus]
      .leftMap { msg => ProblemLoadingProjectStatusFile(file, msg) }
      .toValidationNel
}