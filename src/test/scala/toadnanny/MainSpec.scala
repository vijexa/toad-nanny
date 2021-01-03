package toadnanny

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import cats.implicits._

import Main._

class MainSpec extends AnyFlatSpec {
  "parseArguments" should "correctly parse arguments" in {
    parseArguments(List("--token", "123", "--groupId", "456")) shouldBe 
      Arguments("123", 2000000456, false).asRight

    parseArguments(List("--token", "123", "--groupId", "456", "--debug")) shouldBe
      Arguments("123", 2000000456, true).asRight

    parseArguments(List("--groupId", "456", "--token", "123", "--debug")) shouldBe
      Arguments("123", 2000000456, true).asRight

    parseArguments(List("--debug", "--groupId", "456", "--token", "123")) shouldBe
      Arguments("123", 2000000456, true).asRight

    parseArguments(List("--debug", "--groupId", "456")).isLeft shouldBe true
    parseArguments(List("--debug", "--token", "123")).isLeft shouldBe true
  }
}
