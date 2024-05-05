import zio.*
import zio.direct.*

def saveInformation(info: String): Unit =
  ???

val findTopNewsStory =
  ZIO.succeed:
    "Battery Breakthrough"

def textAlert(message: String) =
  Console.printLine:
    s"Texting story: $message"

import scala.concurrent.Future

var headLineAvailable =
  true
// TODO If we make this function accept the "mock" result and return that, then
//  we can leverage that to hit all of the possible paths in AllTheThings.
def getHeadLine(): Future[String] =
  if (headLineAvailable)
    Future.successful("stock market crash!")
  else
    Future.failed(
      new Exception("Headline not available")
    )

case class HeadlineNotAvailable()
val getHeadlineZ =
  ZIO
    .from:
      getHeadLine()
    .mapError:
      case _: Throwable =>
        HeadlineNotAvailable()

def findTopicOfInterest(
    content: String
): Option[String] =
  Option.when(content.contains("stock market")):
    "stock market"

// TODO Discuss colon clashing in this example
val _: Option[String] =
  findTopicOfInterest:
    "content"

case class NoInterestingTopic()
def topicOfInterestZ(headline: String) =
  ZIO
    .from:
      findTopicOfInterest:
        headline
    .orElseFail:
      NoInterestingTopic()

case class NoRecordsAvailable(topic: String)

import scala.util.Either
def summaryFor(
    topic: String
): Either[NoRecordsAvailable, String] =
  topic match
    case "stock market" =>
      Right:
        s"detailed history of $topic"
    case "obscureTopic" =>
      Left:
        NoRecordsAvailable:
          "obscureTopic"

def summaryForZ(topic: String) =
  ZIO.from:
    summaryFor:
      topic

import scala.util.Try

trait CloseableFile extends AutoCloseable:
  // TODO Return existing entry, rather than a
  // raw Boolean?
  def contains(searchTerm: String): Boolean
  def write(entry: String): Try[String]

def closeableFile() =
  new CloseableFile:
    var contents: List[String] =
      List("Medical Breakthrough!")
    println("Opening file!")
    override def close =
      println("Closing file!")

    override def contains(
        searchTerm: String
    ): Boolean =
      println:
        "Searching file for: " + searchTerm
      searchTerm == "stock market"

    override def write(
        entry: String
    ): Try[String] =
      if (entry == "stock market")
        Try(
          throw new Exception(
            "Stock market already exists!"
          )
        )
      else {
        println("Writing to file: " + entry)
        contents =
          entry :: contents
        Try(entry)
      }

val closeableFileZ =
  ZIO.fromAutoCloseable:
    ZIO.succeed:
      closeableFile()

def writeToFileZ(
    file: CloseableFile,
    content: String
) =
  ZIO
    .from:
      file.write:
        content
    .orDie

val researchWorkflow =
  defer:
    val headline: String =
      getHeadlineZ.run

    val topic: String =
      topicOfInterestZ(headline).run

    val summaryFile: CloseableFile =
      closeableFileZ.run

    val topicIsFresh: Boolean =
      summaryFile.contains:
        topic

    if (topicIsFresh)
      val newInfo =
        summaryForZ(topic).run

      writeToFileZ(summaryFile, newInfo).run
      newInfo
    else
      "Topic was already covered"

object Example06_Composability_0 extends ZIOAppDefault:
  def run =
    defer:
      val topStory =
        findTopNewsStory.run
      textAlert:
        topStory
      .run
  // Texting story: Battery Breakthrough
  // Result: ()


object Example06_Composability_1 extends ZIOAppDefault:
  // TODO Consider deleting .as
  //   The problem is we can't return literals in zio-direct.
  def logAndProvideDefault(e: Throwable) =
    Console
      .printLine:
        e.getMessage
      .as:
        "default value"
  
  def run =
    ZIO
      .attempt:
        ???
      .catchAll:
        logAndProvideDefault
  // an implementation is missing
  // Result: default value


object Example06_Composability_2 extends ZIOAppDefault:
  def run =
    getHeadlineZ
  // Result: stock market crash!


object Example06_Composability_3 extends ZIOAppDefault:
  // This controls some invisible machinery
  headLineAvailable =
    false
  
  def run =
    getHeadlineZ
  // Result: HeadlineNotAvailable()


object Example06_Composability_4 extends ZIOAppDefault:
  // This controls some invisible machinery
  headLineAvailable =
    true
  
  def run =
    topicOfInterestZ:
      "stock market crash!"
  // Result: stock market


object Example06_Composability_5 extends ZIOAppDefault:
  def run =
    topicOfInterestZ:
      "boring and inane content"
  // Result: NoInterestingTopic()


object Example06_Composability_6 extends ZIOAppDefault:
  def run =
    summaryForZ:
      "stock market"
  // Result: detailed history of stock market


object Example06_Composability_7 extends ZIOAppDefault:
  def run =
    summaryForZ:
      "obscureTopic"
  // Result: NoRecordsAvailable(obscureTopic)


object Example06_Composability_8 extends ZIOAppDefault:
  def run =
    closeableFileZ
  // Opening file!
  // Closing file!
  // Result: repl.MdocSession$MdocApp$$anon$18@26e3a84e


object Example06_Composability_9 extends ZIOAppDefault:
  def run =
    defer:
      val file =
        closeableFileZ.run
      file.contains:
        "topicOfInterest"
  // Opening file!
  // Searching file for: topicOfInterest
  // Closing file!
  // Result: false


object Example06_Composability_10 extends ZIOAppDefault:
  def run =
    defer:
      val file =
        closeableFileZ.run
      writeToFileZ(file, "New data on topic").run
  // Opening file!
  // Writing to file: New data on topic
  // Closing file!
  // Result: New data on topic


object Example06_Composability_11 extends ZIOAppDefault:
  def run =
    researchWorkflow
      // todo: some error handling to show that
      // the errors weren't lost along the way
      .mapError:
        case HeadlineNotAvailable() =>
          "Could not fetch headline"
        case NoRecordsAvailable(topic) =>
          s"No records for $topic"
        case NoInterestingTopic() =>
          "No Interesting topic found"
  // Opening file!
  // Searching file for: stock market
  // Writing to file: detailed history of stock market
  // Closing file!
  // Result: detailed history of stock market
