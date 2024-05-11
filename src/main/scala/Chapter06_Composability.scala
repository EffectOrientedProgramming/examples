import zio.*
import zio.direct.*

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

object Chapter06_Composability_0 extends ZIOAppDefault:
  def run =
    getHeadlineZ
  // Result: stock market crash!


object Chapter06_Composability_1 extends ZIOAppDefault:
  // This controls some invisible machinery
  headLineAvailable =
    false
  
  def run =
    getHeadlineZ
  // Result: HeadlineNotAvailable()


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

object Chapter06_Composability_2 extends ZIOAppDefault:
  // This controls some invisible machinery
  headLineAvailable =
    true
  
  def run =
    topicOfInterestZ:
      "stock market crash!"
  // Result: stock market


object Chapter06_Composability_3 extends ZIOAppDefault:
  def run =
    topicOfInterestZ:
      "boring and inane content"
  // Result: NoInterestingTopic()


case class NoRecordsAvailable(topic: String)

import scala.util.Either
def wikiArticle(
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

def wikiArticleZ(topic: String) =
  ZIO.from:
    wikiArticle:
      topic

object Chapter06_Composability_4 extends ZIOAppDefault:
  def run =
    wikiArticleZ:
      "stock market"
  // Result: detailed history of stock market


object Chapter06_Composability_5 extends ZIOAppDefault:
  def run =
    wikiArticleZ:
      "obscureTopic"
  // Result: NoRecordsAvailable(obscureTopic)


import scala.util.Try

trait CloseableFile extends AutoCloseable:
  // TODO Return existing entry, rather than a
  // raw Boolean?
  def contains(searchTerm: String): Boolean
  def write(entry: String): Try[String]
  def summaryFor(searchTerm: String): String

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
      
    override def summaryFor(searchTerm: String): String =
      if (searchTerm == "stock market") 
        "stock markets are neat"
      else
        throw Exception(s"No summary available for $searchTerm")
      
    

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

object Chapter06_Composability_6 extends ZIOAppDefault:
  def run =
    closeableFileZ
  // Opening file!
  // Closing file!
  // Result: repl.MdocSession$MdocApp$$anon$18@3d24925b


object Chapter06_Composability_7 extends ZIOAppDefault:
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


def writeToFileZ(
    file: CloseableFile,
    content: String
) =
  ZIO
    .from:
      file.write:
        content
    .orDie

object Chapter06_Composability_8 extends ZIOAppDefault:
  def run =
    defer:
      val file =
        closeableFileZ.run
      writeToFileZ(file, "New data on topic").run
  // Opening file!
  // Writing to file: New data on topic
  // Closing file!
  // Result: New data on topic


case class NoSummaryAvailable(topic: String) 
def summaryForZ(
    file: CloseableFile,
    // TODO Consider making a CloseableFileZ
    topic: String
) =
  ZIO.attempt:
    file.summaryFor(topic)
  .mapError(_ => NoSummaryAvailable(topic))
    



def summarize(article: String): String =
  println("AI summarizing: start")
  // Represents the AI taking a long time to summarize the content
  if (!article.contains("stock market")) 
    Thread.sleep(1000)
  
  println("AI summarizing: complete")
  s"TODO Summarized content"

case class AIFailure()

def summarizeZ(article: String) =
  ZIO
    .attemptBlockingInterrupt:
      summarize(article)
    .onInterrupt(ZIO.debug("Interrupted summarize"))
    .mapError(_ => AIFailure())

val findTopNewsStory =
  ZIO.succeed:
    "Battery Breakthrough"

def textAlert(message: String) =
  Console.printLine:
    s"Texting story: $message"

object Chapter06_Composability_9 extends ZIOAppDefault:
  def run =
    defer:
      val topStory =
        findTopNewsStory.run
      textAlert(topStory).run
  // Texting story: Battery Breakthrough
  // Result: ()


val researchHeadline =
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
      val wikiArticle =
        wikiArticleZ(topic).run

      val summary = summarizeZ(wikiArticle).run
      writeToFileZ(summaryFile, summary).run
      summary
    else
      summaryForZ(summaryFile, topic).run

object Chapter06_Composability_10 extends ZIOAppDefault:
  def run =
    researchHeadline
      // todo: some error handling to show that
      // the errors weren't lost along the way
      .mapError:
        case HeadlineNotAvailable() =>
          "Could not fetch headline"
        case NoRecordsAvailable(topic) =>
          s"No records for $topic"
        case NoInterestingTopic() =>
          "No Interesting topic found"
        case AIFailure() =>
          "Error during AI summary"
        case NoSummaryAvailable(topic) =>
          s"No summary available for $topic"
  // Opening file!
  // Searching file for: stock market
  // AI summarizing: start
  // AI summarizing: complete
  // Writing to file: TODO Summarized content
  // Closing file!
  // Result: TODO Summarized content


def saveInformation(info: String): Unit =
  ???

object Chapter06_Composability_11 extends ZIOAppDefault:
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
