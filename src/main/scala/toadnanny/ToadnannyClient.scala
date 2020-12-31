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


case class ToadnannyClient [F[_]] (
  client: Client[F], 
  arguments: Arguments
) (implicit
  S : Sync[F]
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
  
  def sendToadInfo: F[Unit] =
    sendMessage("жаба инфо")
}

object ToadnannyClient {

  def getToadStatus (message: DialogMessage): Option[Set[ToadStatus]] = {
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

  def run [F[_]: ConcurrentEffect] (arguments: Arguments) (implicit T: Timer[F], S: Sync[F]): F[ExitCode] = {
    BlazeClientBuilder[F](global).resource.use { client =>
      val tnclient = ToadnannyClient(client, arguments)
      for {
        _ <- tnclient.sendToadInfo
        message <- tnclient.getToadBotMessage
        status = message.flatMap(getToadStatus(_))
        _ <- T.sleep(1.second)
        _ <- tnclient.sendMessage(status.toString)
        _ <- S.delay(println(tnclient.generateUri("messages.send", Map(("message" -> "жаба инфо")))))
        _ <- S.delay(println(status))
      } yield ExitCode.Success
    }
  }
}
