package tracking

import scalaz.ValidationNel

package object model {
  type ProjectId = String
  type EpicId = String
  
  type RepositoryValidation[T] = ValidationNel[RepositoryError, T]
}