package toadnanny

import scala.concurrent.duration._

sealed trait ToadStatus

object ToadStatus {
  case object CanFeed extends ToadStatus
  case class  FeedableIn (time: FiniteDuration) extends ToadStatus

  case object CanTakeFromJob extends ToadStatus
  case class  TakeableFromJobIn (time: FiniteDuration) extends ToadStatus
  case object CanSendToJob extends ToadStatus
  case class  SendableToJobIn (time: FiniteDuration) extends ToadStatus

  case class  TakeableFromDungeonIn (time: FiniteDuration) extends ToadStatus
  case object CanTakeFromDungeon extends ToadStatus
  case object CanSendToDungeon extends ToadStatus
  case class  SendableToDungeonIn (time: FiniteDuration) extends ToadStatus
}
