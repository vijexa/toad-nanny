package toadnanny

import java.net.URLEncoder

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

import cats.effect._
import cats.implicits._
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import toadnanny.Messages.Response._
import toadnanny.ToadStatus._


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

  def getToadBotMessage: F[Either[String, DialogMessage]] = getMessages.map(
    _.head match {
      case m @ DialogMessage(id, _) if id == toadBotId => m.asRight
      case _ => "либо жабабот не ответил либо его перебили".asLeft
    }
  )

  def sendMessage (message: String): F[Unit] =
    client.get(generateUri("messages.send", Map(("message" -> message))))(r => S.unit)

  def getToadStatus: F[Either[String, Set[ToadStatus]]] = for {
    _ <- sendMessage("жаба инфо")
    _ <- T.sleep(1.second)
    messageEither <- getToadBotMessage
  } yield for {
    message <- messageEither
    status <- ToadnannyClient.parseToadStatus(message)
  } yield status

  private def returnMinTime (a: FiniteDuration, b: FiniteDuration): FiniteDuration =
    if (a < b) a else b

  private def sitWithToad: F[Unit] = for {
    _ <- sendMessage("🤖🤖🤖бип боп")
    statusEither <- getToadStatus
    _ <- statusEither match {
      case Right(statusSet) => 
        val (effect, time) = statusSet.foldLeft((S.unit, 12.hours)) { case ((effect, minTime), status) =>
          status match {
            case CanFeed => (
              effect *> sendMessage("покормить жабу"), 
              returnMinTime(5.seconds, minTime)
            )
            case FeedableIn(time) => (effect, returnMinTime(time, minTime))

            case CanTakeFromJob => (
              effect *> sendMessage("завершить работу"),
              returnMinTime(5.seconds, minTime)
            )
            case TakeableFromJobIn(time) => (effect, returnMinTime(time, minTime))
            case CanSendToJob => (
              effect *> sendMessage("отправить жабу на работу"),
              returnMinTime(5.seconds, minTime)
            )
            case SendableToJobIn(time) => (effect, returnMinTime(time, minTime))
          }
        }

        for {
          _ <- effect
          _ <- sendMessage(s"🤖статус был $statusSet, теперь буду ждать $time")
          _ <- T.sleep(time)
          _ <- sitWithToad
        } yield ()
        
      case Left(error) => for {
        _ <- sendMessage(s"🤖 бип-боп что-то пошло не так 😥😥😥\n" +
          s"ошибка: $error" +
          s"\nпопробую еще раз через минуту...")
        _ <- T.sleep(1.minute)
        _ <- sitWithToad
      } yield ()

    }
  } yield ()
}

object ToadnannyClient {

  def parseToadStatus (message: DialogMessage): Either[String, Set[ToadStatus]] = {
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

    if (set.size > 0) set.asRight
    else "не удается запарсить сообщение жабабота".asLeft
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
