package Chapter06_Composability

import zio.*
import zio.direct.*

enum Scenario: // TODO Could these instances _also_ be the error types??
  case StockMarketHeadline
  case HeadlineNotAvailable()
  case NoInterestingTopic()
  case NoWikiArticleAvailable()
  case AITooSlow()

import scala.concurrent.Future

// TODO If we make this function accept the "mock" result and return that, then
//  we can leverage that to hit all of the possible paths in AllTheThings.
def getHeadLine(scenario: Scenario): Future[String] =
  scenario match
      case Scenario.HeadlineNotAvailable() =>
        Future.failed:
          new Exception("Headline not available")
      case Scenario.StockMarketHeadline => 
        Future.successful("stock market crash!")
      case Scenario.NoWikiArticleAvailable() =>
        Future.successful("Fred built a barn.")
      case Scenario.AITooSlow() =>
        Future.successful("space is big!")
    
def findTopicOfInterest(
    content: String
): Option[String] =
  Option.when(content.contains("stock market")):
    "stock market"
  .orElse(
      Option.when(content.contains("space")):
        "space"
  )
  .orElse(
      Option.when(content.contains("barn")):
        "barn"
  )
  
import scala.util.Either
def wikiArticle(
    topic: String
): Either[Scenario.NoWikiArticleAvailable, String] =
  topic match
    case "stock market" | "space" =>
      Right:
        s"detailed history of $topic"
    
    case "barn" =>
      Left:
        Scenario.NoWikiArticleAvailable()

def getHeadlineZ(scenario: Scenario) =
  ZIO
    .from:
      getHeadLine(scenario)
    .mapError:
      case _: Throwable =>
        Scenario.HeadlineNotAvailable()

object Chapter06_Composability_0 extends ZIOAppDefault:
  def run =
    getHeadlineZ(Scenario.StockMarketHeadline)
  // Result: stock market crash!


object Chapter06_Composability_1 extends ZIOAppDefault:
  def run =
    getHeadlineZ(Scenario.HeadlineNotAvailable())
  // Result: HeadlineNotAvailable()


// TODO Discuss colon clashing in this example
val _: Option[String] =
  findTopicOfInterest:
    "content"

def topicOfInterestZ(headline: String) =
  ZIO
    .from:
      findTopicOfInterest:
        headline
    .orElseFail:
      Scenario.NoInterestingTopic()

object Chapter06_Composability_2 extends ZIOAppDefault:
  def run =
    topicOfInterestZ:
      "stock market crash!"
  // Result: stock market


object Chapter06_Composability_3 extends ZIOAppDefault:
  def run =
    topicOfInterestZ:
      "boring and inane content"
  // Result: NoInterestingTopic()


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
  // TODO Handle long line. 
  // Truncating for now: 
  // Defect: scala.MatchError: obscureTopic (of clas
  // Result: Defect: scala.MatchError: obscureTopic (of cla


import scala.util.Try

// TODO Different name to make less confusable with AutoCloseable?
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
      searchTerm == "stock market" || searchTerm == "barn" || searchTerm == "space"
      
      
    override def summaryFor(searchTerm: String): String =
      if (searchTerm == "stock market") 
        "stock markets are neat"
      else if (searchTerm == "space")
        "space is huge"
      else
        throw Exception(s"No summary available for $searchTerm")

    override def write(
        entry: String
    ): Try[String] ={
      // TODO Properly error for an enum case
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
  // Result: repl.MdocSession$MdocApp$$anon$19@24a2eef7


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
  if (article.contains("space")) 
    Thread.sleep(1000)
  
  
  println("AI summarizing: complete")
  if (article.contains("stock market"))
     s"market is not rational"
  else 
    s"TODO summarize $article"


def summarizeZ(article: String) =
  ZIO
    .attemptBlockingInterrupt:
      summarize(article)
    .onInterrupt:
      ZIO.debug("Interrupt AI!")
    .orDie // TODO Confirm we don't care about this case. 
    .timeoutFail(Scenario.AITooSlow())(50.millis)
      

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


def researchHeadlineRaw(scenario: Scenario) =
  defer:
    val headline: String = // Was a Future
      getHeadlineZ(scenario).run

    val topic: String = // Was an Option
      topicOfInterestZ(headline).run 

    val summaryFile: CloseableFile = // Was an AutoCloseable
      closeableFileZ.run

    val topicIsFresh: Boolean =
      summaryFile.contains:
        topic

    if (topicIsFresh)
      val wikiArticle = // Was an Either
        wikiArticleZ(topic).run

      val summary =  // Was slow, blocking
        summarizeZ(wikiArticle).run
        
      // Was a Try
      writeToFileZ(summaryFile, summary).run
      summary
    else
      // Was throwing
      summaryForZ(summaryFile, topic).run

def researchHeadline(scenario: Scenario) =
  researchHeadlineRaw(scenario)
    .mapError:
      case Scenario.HeadlineNotAvailable() =>
        "Could not fetch headline"
      case Scenario.NoInterestingTopic() =>
        "No Interesting topic found"
      case Scenario.AITooSlow() =>
        "Error during AI summary"
      case NoSummaryAvailable(topic) =>
        s"No summary available for $topic"
      case Scenario.NoWikiArticleAvailable() =>
        "No wiki article available"

object Chapter06_Composability_10 extends ZIOAppDefault:
  def run =
    researchHeadline:
      Scenario.StockMarketHeadline
  // Opening file!
  // Searching file for: stock market
  // AI summarizing: start
  // AI summarizing: complete
  // Interrupt AI!
  // Closing file!
  // Result: Error during AI summary


object Chapter06_Composability_11 extends ZIOAppDefault:
  def run =
    researchHeadline:
      Scenario.HeadlineNotAvailable()
  // Result: Could not fetch headline


object Chapter06_Composability_12 extends ZIOAppDefault:
  def run =
    researchHeadline:
      Scenario.NoWikiArticleAvailable()
  // Opening file!
  // Searching file for: barn
  // Closing file!
  // Result: No wiki article available


object Chapter06_Composability_13 extends ZIOAppDefault:
  def run =
    researchHeadline:
      Scenario.AITooSlow()
  // Opening file!
  // Searching file for: space
  // AI summarizing: start
  // Interrupt AI!
  // Closing file!
  // Result: Error during AI summary


def saveInformation(info: String): Unit =
  ???

object Chapter06_Composability_14 extends ZIOAppDefault:
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
