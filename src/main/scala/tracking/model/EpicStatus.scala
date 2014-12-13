package tracking.model

import scalaz.Equal._

sealed trait EpicStatus

object EpicStatus {
  implicit val epicEqual = equalA[EpicStatus]
}

case object NotStarted extends EpicStatus
case object InProgress extends EpicStatus
case object Complete extends EpicStatus