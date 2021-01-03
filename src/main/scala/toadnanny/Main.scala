package toadnanny

import cats.effect.{IO, IOApp}
import cats.effect.ExitCode

object Main extends IOApp {

  def getArguments (args: List[String]): Map[String, String] = 
    args.sliding(2).collect{
      case List(argName, arg) => (argName -> arg)
    }.toMap

  def parseArguments (args: List[String]): Either[String, Arguments] = {
    val argumentMap = getArguments(args)
    for {
      token   <- argumentMap.get("--token").toRight("specify --token argument")
      groupId <- argumentMap.get("--groupId").flatMap(_.toLongOption)
        .toRight("specify --groupId argument")
      isDebug =  argumentMap.exists{ case (a, b) => a == "--debug" || b == "--debug" }
      _ = println(isDebug)
    } yield Arguments(token, 2_000_000_000 + groupId, isDebug)
  }

  def run(args: List[String]) = {
    parseArguments(args) match {
      case Left(error) => IO(println(error)) as ExitCode.Error
      case Right(arguments) => ToadnannyClient.run[IO](arguments)
    }
  }
}
