
package object toadnanny {
  case class Arguments (token: String, groupId: Long, isDebug: Boolean)

  val toadBotId: Long = -191097210

  val feedableInRegex = """Можно покормить через (\d+)ч:(\d+)м""".r.unanchored
  val canFeedRegex = """Жабу можно покормить""".r.unanchored

  val takeableFromJobInRegex = """Забрать жабу можно через (\d+)ч:(\d+)м""".r.unanchored
  val canTakeFromJobRegex = """Можно забрать жабу с работы""".r.unanchored
  val sendableToJobInRegex = """Отправить на работу можно будет через (\d+)ч:(\d+)м""".r.unanchored
  val canSendToJobRegex = """Жабу можно отправить на работу""".r.unanchored

  val takeableFromDungeonInRegex = """Выйти из подземелья можно через (\d+)мин""".r.unanchored
  val canTakeFromDungeonRegex = """Можно выйти из подземелья""".r.unanchored
  val canSendToDungeonRegex = """Можно отправиться в подземелье""".r.unanchored
  val sendableToDungeonInRegex = """В подземелье можно через (\d+)ч. (\d+)м.""".r.unanchored
}
