package toadnanny

import java.net.URLEncoder

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

import cats.effect._
import cats.implicits._
import io.circe.parser.decode
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

  private def generateUri (method: String, args: Map[String, String] = Map.empty): String =
    s"https://api.vk.com/method/$method?" +
      s"v=5.52&" +
      s"access_token=${arguments.token}&" +
      s"peer_id=${arguments.groupId}&" +
      args.map{ case (name, value) => 
        s"$name=${URLEncoder.encode(value)}"
      }.mkString("&")

  private def getMessages: F[Either[String, List[DialogMessage]]] =
    client.get(generateUri("messages.getHistory"))(r =>
      r.bodyText.compile.toList.map(body =>
        decode[GetHistory](body.mkString).map(_.response.items)
          .leftMap(err => s"неожиданный респонс при попытке получить сообщения: $err")
      )
    )

  private def getToadBotMessage (messageId: Long): F[Either[String, DialogMessage]] = for {
    messagesEither <- getMessages
  } yield for {
    messages <- messagesEither
    toadBotMessage <- messages.sliding(2).collectFirst {
      case List(message @ DialogMessage(_, fromId, _), DialogMessage(id, _, body)) 
        if id == messageId && fromId == toadBotId => message
    }.toRight("либо жабабот не ответил либо его перебили")
  } yield toadBotMessage

  private def sendMessage (message: String): F[Either[String, Long]] =
    client.get(
      generateUri("messages.send", Map(("message" -> message)))
    )(r => 
      r.bodyText.compile.toList.map(body =>
        decode[SendMessageResponse](body.mkString).map(_.response)
          .leftMap(err => s"неожиданный респонс при попытке отправить сообщение: $err")
      )
    )

  private def getToadStatus: F[Either[String, Set[ToadStatus]]] = for {
    messageIdEither <- sendMessage("жаба инфо")
    _ <- T.sleep(1.second)
    messageEither <- messageIdEither.map(messageId => getToadBotMessage(messageId))
      .sequence.map(_.flatten)
  } yield for {
    message <- messageEither
    status <- ToadnannyClient.parseToadStatus(message)
  } yield status

  private def returnMinTime (a: FiniteDuration, b: FiniteDuration): FiniteDuration =
    if (a < b) a else b

  private def sitWithToad: F[Unit] = for {
    _ <- if (arguments.isDebug) sendMessage("🤖🤖🤖бип боп") else S.unit
    statusEither <- getToadStatus
    _ <- statusEither match {
      case Right(statusSet) => 
        val (effect, time) = statusSet.foldLeft((S.unit, 12.hours)) { case ((effect, minTime), status) =>
          status match {
            case CanFeed => (
              effect *> sendMessage("покормить жабу") as (), 
              returnMinTime(5.seconds, minTime)
            )
            case FeedableIn(time) => (effect, returnMinTime(time, minTime))

            case CanTakeFromJob => (
              effect *> sendMessage("завершить работу") as (),
              returnMinTime(5.seconds, minTime)
            )
            case TakeableFromJobIn(time) => (effect, returnMinTime(time, minTime))
            case CanSendToJob => (
              effect *> sendMessage("отправить жабу на работу") as (),
              returnMinTime(5.seconds, minTime)
            )
            case SendableToJobIn(time) => (effect, returnMinTime(time, minTime))
          }
        }

        for {
          _ <- effect
          _ <- if (arguments.isDebug) {
            sendMessage(s"🤖статус был $statusSet, теперь буду ждать $time") 
          } else {
            S.unit
          }
          _ <- T.sleep(time)
          _ <- sitWithToad
        } yield ()
        
      case Left(error) => for {
        _ <- if (arguments.isDebug) {
          sendMessage(s"🤖 бип-боп что-то пошло не так 😥😥😥\n" +
            s"ошибка: $error\n" +
            s"попробую еще раз через минуту...")
        } else {
          S.unit
        }
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
