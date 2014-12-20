package tracking.repository

import java.io.File

sealed trait RepositoryLoadError

case class ProblemLoadingProjectStatusFile(file: File, parseError: String) extends RepositoryLoadError
case class ProblemLoadingMetaFile(file: File, parseError: String) extends RepositoryLoadError
