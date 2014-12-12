package tracking.repository

import java.io.File

sealed trait RepositoryLoadError

case class ProjectDirectoryNameParseFailure(file: File) extends RepositoryLoadError
case class ProblemLoadingProjectStatusFile(file: File, parseError: String) extends RepositoryLoadError
