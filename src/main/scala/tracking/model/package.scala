package tracking

import scalaz.ValidationNel

package object model {
  type ProjectId = String
  type EpicId = String
  
  type Epic = IdentifierAndTitle
  
  type RepositoryValidation[T] = ValidationNel[RepositoryError, T]
}