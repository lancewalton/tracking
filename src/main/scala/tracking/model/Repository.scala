package tracking.model

import org.joda.time.LocalDate

import scalaz.NonEmptyList
import scalaz.std.list._
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.validation._

case class Repository(projects: List[Project])

object Repository {
  def createValid(projects: List[Project]): RepositoryValidation[Repository] =
    toValidationNel(accumulateValidationErrors(projects), projects).map(Repository(_))
  
  private def accumulateValidationErrors(projects: List[Project]): List[RepositoryError] =
    validateProjectNames(projects) ::: validateProjectIds(projects) ::: (projects >>= validateProject) ::: validateDependencies(projects).toList
    
  private def validateProjectNames(projects: List[Project]): List[RepositoryError] =
    validateUniqueAndNonEmptyString(projects, (_: Project).identifiers.title, DuplicateProjectName.apply _, EmptyProjectName(_: Project))
  
  private def validateProjectIds(projects: List[Project]): List[RepositoryError] =
    validateUniqueAndNonEmptyString(projects, (_: Project).identifiers.id, DuplicateProjectId.apply _, EmptyProjectId(_: Project))
  
  private def validateProject(project: Project): List[RepositoryError] =
    validateAttributeIsUnique(project.statuses, (_: ProjectStatus).date, (d: LocalDate) => DuplicateReportStatusDate(project, d)) :::
      (project.statuses >>= validateProjectStatus(project))
    
  private def validateProjectStatus(project: Project)(status: ProjectStatus): List[RepositoryError] =
    validateUniqueAndNonEmptyString(status.epics, (_: Epic).title, (n: String) => DuplicateEpicTitle(project, status, n), EmptyEpicTitle(project, status, _: Epic)) :::
    validateUniqueAndNonEmptyString(status.epics, (_: Epic).id, (n: String) => DuplicateEpicId(project, status, n), EmptyEpicId(project, status, _: Epic))
    
  private def validateDependencies(projects: List[Project]): List[RepositoryError] =
    for {
      project <- projects
      status <- project.statuses
      dependency <- status.dependencies
      validation <- validateDependency(projects, project, status, dependency)
    } yield validation
    
  private def validateDependency(projects: List[Project], project: Project, status: ProjectStatus, dependency: Dependency): Option[RepositoryError] =
    for {
      p ← projects.find(_.identifiers.id === dependency.projectId)
      v ← validateDependency(project, status, dependency, p).orElse(Option(DependencyRefersToUnknownProject(project, status, dependency)))
    } yield v

  private def validateDependency(project: Project, status: ProjectStatus, dependency: Dependency, projectDependedUpon: Project): Option[RepositoryError] =
    project
      .findEpic(status.date, dependency.epicId)
      .fold(Option(DependencyRefersToUnknownEpic(project, status, dependency)))(_ => None)

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