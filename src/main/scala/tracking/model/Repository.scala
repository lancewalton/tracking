package tracking.model

import org.joda.time.LocalDate

import scalaz.NonEmptyList
import scalaz.std.list._
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.validation._

case class Repository(projects: List[Project]) {
  def dates = projects.flatMap { _.statuses.map { _.date } }
  
  def project(id: ProjectId): Option[Project] = projects.find(_.meta.identifiers.id === id)
  
  def reachableEpics(project: Project, date: LocalDate): Set[(Project, Epic)] =
    (for {
      status <- project.status(date).toList
      e <- status.epics
      de = e.dependencies.flatMap(d => reachableEpics(d, date))
    } yield (project, e) :: de).flatten.toSet
  
  private def reachableEpics(dependency: Dependency, date: LocalDate): Set[(Project, Epic)] =
    (for {
      p <- project(dependency.projectId).toList
      e <- p.findEpic(date, dependency.epicId)
      de = e.dependencies.flatMap(d => reachableEpics(d, date))
    } yield (p, e) :: de).flatten.toSet
}

object Repository {
  def createValid(projects: List[Project]): RepositoryValidation[Repository] =
    toValidationNel(accumulateValidationErrors(projects), projects).map(Repository(_))
  
  private def accumulateValidationErrors(projects: List[Project]): List[RepositoryError] =
    validateProjectNames(projects) ::: validateProjectIds(projects) ::: (projects >>= validateProject) ::: validateDependencies(projects).toList
    
  private def validateProjectNames(projects: List[Project]): List[RepositoryError] =
    validateUniqueAndNonEmptyString(projects, Project.titleLens.get, DuplicateProjectName.apply _, EmptyProjectName(_: Project))
  
  private def validateProjectIds(projects: List[Project]): List[RepositoryError] =
    validateUniqueAndNonEmptyString(projects, Project.idLens.get, DuplicateProjectId.apply _, EmptyProjectId(_: Project))
  
  private def validateProject(project: Project): List[RepositoryError] =
    validateAttributeIsUnique(project.statuses, (_: ProjectStatus).date, (d: LocalDate) => DuplicateReportStatusDate(project, d)) :::
      (project.statuses >>= validateProjectStatus(project))
    
  private def validateProjectStatus(project: Project)(status: ProjectStatus): List[RepositoryError] =
    validateUniqueAndNonEmptyString(status.epics, (_: Epic).identifiers.title, (n: String) => DuplicateEpicTitle(project, status, n), EmptyEpicTitle(project, status, _: Epic)) :::
    validateUniqueAndNonEmptyString(status.epics, (_: Epic).identifiers.id, (n: String) => DuplicateEpicId(project, status, n), EmptyEpicId(project, status, _: Epic))
    
  private def validateDependencies(projects: List[Project]): List[RepositoryError] =
    for {
      project <- projects
      status <- project.statuses
      epic <- status.epics
      dependency <- epic.dependencies
      validation <- validateDependency(projects, project, status, epic, dependency)
    } yield validation
    
  private def validateDependency(projects: List[Project], project: Project, status: ProjectStatus, epic: Epic, dependency: Dependency): Option[RepositoryError] =
    projects.find(Project.idLens.get(_) === dependency.projectId)
      .fold(Option(DependencyRefersToUnknownProject(project, status, epic, dependency).asInstanceOf[RepositoryError])) { p =>
        validateDependency(project, status, epic, dependency, p)
      }

  private def validateDependency(project: Project, status: ProjectStatus, epic: Epic, dependency: Dependency, projectDependedUpon: Project): Option[RepositoryError] =
    projectDependedUpon
      .findEpic(status.date, dependency.epicId)
      .fold(Option(DependencyRefersToUnknownEpic(project, status, epic, dependency)))(_ => None)

  private def validateUniqueAndNonEmptyString[C](collection: List[C], extractor: C => String, duplicateError: String => RepositoryError, emptyError: C => RepositoryError): List[RepositoryError] =
    validateAttributeIsUnique(collection, extractor, duplicateError) ::: validateStringAttributesAreNonEmpty(collection, extractor, emptyError).toList
    
  private def validateAttributeIsUnique[C, T](collection: List[C], extractor: C => T, duplicateError: T => RepositoryError): List[RepositoryError] =
    collection
      .groupBy(extractor)
      .filter(_._2.size > 1).map(_._1)
      .map(duplicateError(_))
      .toList
      
  private def validateStringAttributesAreNonEmpty[C](collection: List[C], extractor: C => String, error: C => RepositoryError) =
    collection
      .map(t => (t, extractor(t)))
      .filter(_._2.trim.isEmpty)
      .map(_._1)
      .map(error)
      .distinct
      
  private def toValidationNel[S](errors: List[RepositoryError], success: => S): RepositoryValidation[S] =
    errors match {
      case Nil => success.successNel[RepositoryError]
      case h :: t => NonEmptyList[RepositoryError](h, t: _*).failure[S]
    }
}