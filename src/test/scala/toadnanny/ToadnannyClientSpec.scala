package toadnanny

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import cats.implicits._

import scala.concurrent.duration._

import ToadnannyClient._
import toadnanny.ToadStatus._
import toadnanny.Messages.Response.DialogMessage

class ToadnannyClientSpec extends AnyFlatSpec {
  "parseToadStatus" should "correctly parse toadboat message" in {
    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|🍭:Можно покормить через 9ч:23м
           |(Откормить через 1ч:23м)
           |🏃‍♂:Отправить на работу можно будет через 5ч:24м
           |⚔:Не участвует в дуэли
           |☠:В подземелье можно через 2ч. 16м.
           |💃:Можно пойти на тусу
           |💘:Жаба не в браке""".stripMargin
      )
    ) shouldBe Set(
      SendableToJobIn(5.hours + 24.minutes), 
      FeedableIn(9.hours + 23.minutes),
      SendableToDungeonIn(136.minutes)
    ).asRight

    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|🍭:Можно покормить через 0ч:0м
           |(Откормить через 1ч:23м)
           |🏃‍♂:Отправить на работу можно будет через 0ч:0м
           |⚔:Не участвует в дуэли
           |☠:Можно отправиться в подземелье
           |💃:Можно пойти на тусу
           |💘:Жаба не в браке""".stripMargin
      )
    ) shouldBe Set(
      SendableToJobIn(0.minutes), 
      FeedableIn(0.minutes),
      CanSendToDungeon
    ).asRight

    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|🍭:Жабу можно покормить
           |(Можно откормить)
           |🏃‍♂:Жабу можно отправить на работу
           |⚔:Не участвует в дуэли
           |☠:Можно выйти из подземелья
           |💃:Можно пойти на тусу
           |💘:Жаба не в браке""".stripMargin
      )
    ) shouldBe Set(
      CanFeed, 
      CanSendToJob,
      CanTakeFromDungeon
    ).asRight

    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|🍭:Можно покормить через 11ч:60м
           |(Откормить через 3ч:60м)
           |🏃‍♂:Забрать жабу можно через 1ч:60м
           |⚔:Не участвует в дуэли
           |☠:Выйти из подземелья можно через 42мин.
           |💃:Можно пойти на тусу
           |💘:Жаба не в браке""".stripMargin
      )
    ) shouldBe Set(
      FeedableIn(720.minutes), 
      TakeableFromJobIn(120.minutes),
      TakeableFromDungeonIn(42.minutes)
    ).asRight

    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|🍭:Можно покормить через 5ч:53м
           |(Можно откормить)
           |🏃‍♂:Можно забрать жабу с работы
           |⚔:Не участвует в дуэли
           |💃:Можно пойти на тусу
           |💘:Жаба не в браке""".stripMargin
      )
    ) shouldBe Set(FeedableIn(353.minutes), CanTakeFromJob).asRight

    parseToadStatus(
      DialogMessage(
        0,
        0,
        """|Ваш инвентарь:
           |🍬Леденцы: 0
           |💊Аптечки: 1
           |🍻Пивас: Жабка без пива :(
           |🦟Стрекозюля удачи: Ее нет:(
           |🗺Карта болота: 1""".stripMargin
      )
    ).isLeft shouldBe true
  }

  "returnMinTime" should "return the minimal time out of two" in {
    returnMinTime(1.second, 1.hour) shouldBe 1.second
    returnMinTime(120.minutes, 1.hour) shouldBe 1.hour
  }
}
