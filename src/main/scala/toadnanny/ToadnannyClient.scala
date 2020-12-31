package toadnanny

import cats.effect.{ConcurrentEffect, Timer}
import cats.implicits._
import org.http4s.client.blaze.BlazeClientBuilder
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._
import cats.effect.ExitCode
import org.http4s.client.Client
import org.http4s.circe._

import toadnanny.Messages.Response._
import cats.effect.Sync
import org.http4s.Uri
import java.net.URLEncoder
import scala.annotation.tailrec
import toadnanny.ToadStatus.CanFeed
import toadnanny.ToadStatus.FeedableIn
import toadnanny.ToadStatus.CanTakeFromJob
import toadnanny.ToadStatus.TakeableFromJobIn
import toadnanny.ToadStatus.CanSendToJob
import toadnanny.ToadStatus.SendableToJobIn
import cats.effect.Concurrent


case class ToadnannyClient [F[_]] (
  client: Client[F], 
  arguments: Arguments
) (implicit
  S : Sync[F],
  T : Timer[F],
  C : Concurrent[F]
) {

  def generateUri (method: String, args: Map[String, String] = Map.empty): Uri =
    Uri.unsafeFromString(
      s"https://api.vk.com/method/$method?" +
        s"v=5.52&" +
        s"access_token=${arguments.token}&" +
        s"peer_id=${arguments.groupId}&" +
        args.map{ case (name, value) => 
          s"$name=${URLEncoder.encode(value)}"
        }.mkString("&")
    )

  def getMessages: F[List[DialogMessage]] = {
    implicit val getHistoryEntityDecoder = jsonOf[F, GetHistory]
    client.expect[GetHistory](generateUri("messages.getHistory"))
      .map(_.response.items)
  }

  def getToadBotMessage: F[Option[DialogMessage]] = getMessages.map(
    _.head match {
      case m @ DialogMessage(id, _) if id == toadBotId => m.some
      case _ => None
    }
  )

  def sendMessage (message: String): F[Unit] =
    client.get(generateUri("messages.send", Map(("message" -> message))))(r => S.unit)

  def getToadStatus: F[Option[Set[ToadStatus]]] = for {
    _ <- sendMessage("жаба инфо")
    _ <- T.sleep(1.second)
    messageOpt <- getToadBotMessage
  } yield for {
    message <- messageOpt
    status <- ToadnannyClient.parseToadStatus(message)
  } yield status

  private def returnMinTime (a: FiniteDuration, b: FiniteDuration): FiniteDuration =
    if (a < b) a else b

  private def sitWithToad: F[Unit] = for {
    statusOpt <- getToadStatus
    _ <- statusOpt match {
      case Some(statusSet) => 
        val (effect, time) = statusSet.foldLeft((S.unit, 12.hours)) { case ((effect, minTime), status) =>
          status match {
            case CanFeed => (
              effect *> sendMessage("покормить жабу"), 
              returnMinTime(12.hours, minTime)
            )
            case FeedableIn(time) => (effect, returnMinTime(time, minTime))

            case CanTakeFromJob => (
              effect *> sendMessage("завершить работу"),
              returnMinTime(6.hours, minTime)
            )
            case TakeableFromJobIn(time) => (effect, returnMinTime(time, minTime))
            case CanSendToJob => (
              effect *> sendMessage("отправить жабу на работу"),
              returnMinTime(2.hours, minTime)
            )
            case SendableToJobIn(time) => (effect, returnMinTime(time, minTime))
          }
        }

        for {
          _ <- effect
          _ <- sendMessage(s"🤖статус был $statusSet, тепер буду ждатб $time")
          _ <- T.sleep(time + 5.minutes)
          _ <- sitWithToad
        } yield ()
        
      case None => sendMessage("🤖бип-боп шота пашло нитак памогите 😥😥😥")
    }
  } yield ()
}

object ToadnannyClient {

  def parseToadStatus (message: DialogMessage): Option[Set[ToadStatus]] = {
    import ToadStatus._

    val set: Set[ToadStatus] = message.body.split("\n").flatMap(_ match {
      case canFeedRegex() => CanFeed.some
      case feedableInRegex(hours, minutes) => 
        FeedableIn(hours.toInt.hours + minutes.toInt.minutes).some

      case canSendToJobRegex() => CanSendToJob.some
      case sendableToJobInRegex(hours, minutes) =>
        SendableToJobIn(hours.toInt.hours + minutes.toInt.minutes).some
      case canTakeFromJobRegex() => CanTakeFromJob.some
      case takeableFromJobInRegex(hours, minutes) =>
        TakeableFromJobIn(hours.toInt.hours + minutes.toInt.minutes).some

      case _ => None
    }).toSet

    if (set.size > 0) set.some
    else None
  }

  def run [F[_]: ConcurrentEffect] (
    arguments: Arguments
  ) (implicit 
    T: Timer[F], 
    S: Sync[F]
  ): F[ExitCode] = {
    BlazeClientBuilder[F](global).resource.use { client =>
      val tnclient = ToadnannyClient(client, arguments)
      for {
        status <- tnclient.sitWithToad
      } yield ExitCode.Success
    }
  }
}
