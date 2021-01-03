package toadnanny

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import io.circe.parser.decode

import cats.implicits._

import Messages.Response._

class MessagesSpec extends AnyFlatSpec {
  "GetHistory codec" should "decode json correctly" in {
    decode[GetHistory](
      """
        |{
        |"response": {
        |"count": 398,
        |"items": [{
        |"id": 1680163,
        |"body": "test message",
        |"user_id": 2314852,
        |"from_id": 2314852,
        |"date": 1468343751,
        |"read_state": 1,
        |"out": 0
        |}],
        |"in_read": 1680163,
        |"out_read": 1680162
        |}
        }""".stripMargin
    ) shouldBe GetHistory(
      GetHistoryResponse(
        List(DialogMessage(
          id = 1680163, 
          fromId = 2314852, 
          body = "test message"
        ))
      )
    ).asRight
  }

  "SendMessageResponse codec" should "decode json correctly" in {
    decode[SendMessageResponse](
      """|{
         |"response": 9497009
         |}""".stripMargin
    ) shouldBe SendMessageResponse(response = 9497009).asRight
  }
}
