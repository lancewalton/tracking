package tracking

import scalaz.ValidationNel

import tracking.repository.RepositoryLoadError

package object repository {
  type LoadedRepository[T] = ValidationNel[RepositoryLoadError, T]
}