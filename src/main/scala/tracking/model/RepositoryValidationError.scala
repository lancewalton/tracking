package tracking.model

import org.joda.time.LocalDate

sealed trait RepositoryError

case class DuplicateProjectName(name: String) extends RepositoryError
case class EmptyProjectName(project: Project) extends RepositoryError
case class DuplicateProjectId(id: String) extends RepositoryError
case class EmptyProjectId(project: Project) extends RepositoryError
case class DuplicateReportStatusDate(project: Project, date: LocalDate) extends RepositoryError
case class EmptyEpicTitle(project: Project, projectStatus: ProjectStatus, epic: Epic) extends RepositoryError
case class DuplicateEpicTitle(project: Project, projectStatus: ProjectStatus, name: String) extends RepositoryError
case class EmptyEpicId(project: Project, projectStatus: ProjectStatus, epic: Epic) extends RepositoryError
case class DuplicateEpicId(project: Project, projectStatus: ProjectStatus, id: String) extends RepositoryError
case class DependencyRefersToUnknownProject(project: Project, status: ProjectStatus, dependency: Dependency) extends RepositoryError
case class DependencyRefersToUnknownEpic(project: Project, status: ProjectStatus, dependency: Dependency) extends RepositoryError