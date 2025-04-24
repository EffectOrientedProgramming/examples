import zio.*
import zio.direct.*
import zio.http.{Client, Request}

object Gibberish extends ZIOAppDefault:

  val randomNumUrl = "https://random-num-374286597410.us-central1.run.app"
  val randomWordUrl = "https://random-word-374286597410.us-central1.run.app"

  val randomNum =
    defer:
      val response = Client.batched(Request.get(randomNumUrl)).run
      response.body.asString.run.toInt

  val randomWord =
    defer:
      val response = Client.batched(Request.get(randomWordUrl)).run
      response.body.asString.run

  val sentence =
    defer:
      val num = randomNum.run
      val words = ZIO.collectAllPar:
        Seq.fill(num)(randomWord)
      words.run.mkString(" ")

  def run =
    sentence.debug.provide(Client.default)