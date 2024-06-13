package Chapter06_Composability

import zio.*
import zio.direct.*

enum Scenario: // TODO Could these instances _also_ be the error types??
  case StockMarketHeadline
  case HeadlineNotAvailable
  case NoInterestingTopic()
  // There is an Either[NoWikiArticleAvailable,_]
  // in visible code, so if we make it an object,
  // It will be
  // Either[NoWikiArticleAvailable.type,_] :(
  case NoWikiArticleAvailable()
  case AITooSlow()
  case SummaryReadThrows()
  case DiskFull()

import scala.concurrent.Future
// TODO If we make this function accept the "mock" result and return that, then
//  we can leverage that to hit all of the possible paths in AllTheThings.
def getHeadLine(
    scenario: Scenario
): Future[String] =
  println("Network - Getting headline")
  scenario match
    case Scenario.HeadlineNotAvailable =>
      Future.failed:
        new Exception("Headline not available")
    case Scenario.StockMarketHeadline =>
      Future.successful("stock market rising!")
    case Scenario.NoWikiArticleAvailable() =>
      Future.successful("Fred built a barn.")
    case Scenario.AITooSlow() =>
      Future.successful("space is big!")
    case Scenario.SummaryReadThrows() =>
      Future.successful("new unicode released!")
    case Scenario.NoInterestingTopic() =>
      Future.successful(
        "TODO Use boring content here"
      )
    case Scenario.DiskFull() =>
      Future.successful("human genome sequenced")
end getHeadLine

def findTopicOfInterest(
    content: String
): Option[String] =
  // TODO Decide best output string here
  println("Analytics - Scanning")
  val topics =
    List(
      "stock market",
      "space",
      "barn",
      "unicode",
      "genome"
    )
  topics.find(content.contains)

import scala.util.Either
def wikiArticle(topic: String): Either[
  Scenario.NoWikiArticleAvailable,
  String
] =
  println(s"Wiki - articleFor($topic)")
  topic match
    case "stock market" | "space" | "genome" =>
      Right:
        s"detailed history of $topic"

    case "barn" =>
      Left:
        Scenario.NoWikiArticleAvailable()

import scala.concurrent.Future

def getHeadlineZ(scenario: Scenario) =
  ZIO
    .from:
      getHeadLine(scenario)
    .mapError:
      case _: Throwable =>
        Scenario.HeadlineNotAvailable

object App0 extends helpers.ZIOAppDebug:
  def run =
    getHeadlineZ(Scenario.StockMarketHeadline)
  // Network - Getting headline
  // Result: stock market rising!


object App1 extends helpers.ZIOAppDebug:
  def run =
    getHeadlineZ(Scenario.HeadlineNotAvailable)
  // Network - Getting headline
  // Result: HeadlineNotAvailable


val result: Option[String] =
  findTopicOfInterest:
    "content"

def topicOfInterestZ(headline: String) =
  ZIO
    .from:
      findTopicOfInterest:
        headline
    .orElseFail:
      Scenario.NoInterestingTopic()

object App2 extends helpers.ZIOAppDebug:
  def run =
    topicOfInterestZ:
      "stock market rising!"
  // Analytics - Scanning
  // Result: stock market


object App3 extends helpers.ZIOAppDebug:
  def run =
    topicOfInterestZ:
      "boring and inane content"
  // Analytics - Scanning
  // Result: NoInterestingTopic()


def wikiArticleZ(topic: String) =
  ZIO.from:
    wikiArticle:
      topic

object App4 extends helpers.ZIOAppDebug:
  def run =
    wikiArticleZ:
      "stock market"
  // Wiki - articleFor(stock market)
  // Result: detailed history of stock market


object App5 extends helpers.ZIOAppDebug:
  def run =
    wikiArticleZ:
      "barn"
  // Wiki - articleFor(barn)
  // Result: NoWikiArticleAvailable()


import scala.util.Try

// TODO Different name to make less confusable with AutoCloseable?
trait File extends AutoCloseable:
  // TODO Return existing entry, rather than a
  // raw Boolean?
  def contains(searchTerm: String): Boolean
  def write(entry: String): Try[String]
  def summaryFor(searchTerm: String): String
  def sameContent(other: File): Boolean
  def content(): String

def openFile(path: String) =
  new File:
    var contents: List[String] =
      List("Medical Breakthrough!")
    println("File - OPEN")

    override def content() =
      path match
        case "file1.txt" | "file2.txt" =>
          "hot dog"
        case _ =>
          "not hot dog"

    override def sameContent(
        other: File
    ): Boolean =
      println(
        "side-effect print: comparing content"
      )
      content() == other.content()

    override def close =
      println("File - CLOSE")

    override def contains(
        searchTerm: String
    ): Boolean =
      println:
        s"File - contains($searchTerm)"

      // todo use path to determine behavior?
      searchTerm match
        case "wheel" | "unicode" =>
          true
        case _ =>
          false

    override def summaryFor(
        searchTerm: String
    ): String =
      println(s"File - summaryFor($searchTerm)")
      if (searchTerm == "unicode")
        throw Exception(
          s"No summary available for $searchTerm"
        )
      else if (searchTerm == "stock market")
        "stock markets are neat"
      else if (searchTerm == "space")
        "space is huge"
      else
        ???

    override def write(
        entry: String
    ): Try[String] =
      if (entry.contains("genome")) {
        println("File - disk full!")
        Try(throw new Exception("Disk is full!"))
      } else {
        println("File - write: " + entry)
        contents =
          entry :: contents
        Try(entry)
      }

def openFileZ(path: String) =
  ZIO.fromAutoCloseable:
    ZIO.succeed:
      openFile(path)

object App6 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file =
        openFileZ("file1.txt").run
      file.contains:
        "topicOfInterest"
  // File - OPEN
  // File - contains(topicOfInterest)
  // File - CLOSE
  // Result: false


object App7 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file1 =
        openFileZ("file1.txt").run
      val file2 =
        openFileZ("file2.txt").run
      file1.sameContent(file2)
  // File - OPEN
  // File - OPEN
  // side-effect print: comparing content
  // File - CLOSE
  // File - CLOSE
  // Result: true


def writeToFileZ(file: File, content: String) =
  ZIO
    .from:
      file.write:
        content
    .mapError:
      _ => Scenario.DiskFull()

object App8 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file =
        openFileZ("file1").run
      writeToFileZ(file, "New data on topic").run
  // File - OPEN
  // File - write: New data on topic
  // File - CLOSE
  // Result: New data on topic


case class NoSummaryAvailable(topic: String)
def summaryForZ(
    file: File,
    // TODO Consider making a CloseableFileZ
    topic: String
) =
  ZIO
    .attempt:
      file.summaryFor(topic)
    .mapError:
      _ => NoSummaryAvailable(topic)

def summarize(article: String): String =
  println(s"AI - summarize - start")
  // Represents the AI taking a long time to
  // summarize the content
  if (article.contains("space"))
    // This should go away when our clock is less
    // dumb
    println(
      "printing because our test clock is insane"
    )
    Thread.sleep(1000)

  println(s"AI - summarize - end")
  if (article.contains("stock market"))
    s"market is not rational"
  else if (article.contains("genome"))
    "The human genome is huge!"
  else if (article.contains("topic"))
    "topic summary"
  else
    ???
end summarize

// TODO Can we use silent instead of compile-only above?
val summary: String =
  summarize("topic")

def summarizeZ(article: String) =
  ZIO
    .attemptBlockingInterrupt:
      summarize(article)
    .onInterrupt:
      ZIO.debug("AI **INTERRUPTED**")
    .orDie // TODO Confirm we don't care about this case.
    .timeoutFail(Scenario.AITooSlow())(50.millis)

val findTopNewsStory =
  ZIO.succeed:
    "Battery Breakthrough"

def textAlert(message: String) =
  Console.printLine:
    s"Texting story: $message"

object App9 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val topStory =
        findTopNewsStory.run
      textAlert(topStory).run
  // Texting story: Battery Breakthrough


def researchHeadline(scenario: Scenario) =
  defer:
    val headline: String =
      getHeadlineZ(scenario).run

    val topic: String =
      topicOfInterestZ(headline).run

    val summaryFile: File =
      // TODO Use Scenario to determine file?
      openFileZ("file1.txt").run

    val knownTopic: Boolean =
      summaryFile.contains:
        topic

    if (knownTopic)
      summaryForZ(summaryFile, topic).run
    else
      val wikiArticle: String =
        wikiArticleZ(topic).run

      val summary: String =
        summarizeZ(wikiArticle).run

      writeToFileZ(summaryFile, summary).run
      summary

object App10 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.HeadlineNotAvailable
  // Network - Getting headline
  // Result: HeadlineNotAvailable


object App11 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.NoInterestingTopic()
  // Network - Getting headline
  // Analytics - Scanning
  // Result: NoInterestingTopic()


object App12 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.SummaryReadThrows()
  // Network - Getting headline
  // Analytics - Scanning
  // File - OPEN
  // File - contains(unicode)
  // File - summaryFor(unicode)
  // File - CLOSE
  // Result: NoSummaryAvailable(unicode)


object App13 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.NoWikiArticleAvailable()
  // Network - Getting headline
  // Analytics - Scanning
  // File - OPEN
  // File - contains(barn)
  // Wiki - articleFor(barn)
  // File - CLOSE
  // Result: NoWikiArticleAvailable()


object App14 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.AITooSlow()
  // Network - Getting headline
  // Analytics - Scanning
  // File - OPEN
  // File - contains(space)
  // Wiki - articleFor(space)
  // AI - summarize - start
  // printing because our test clock is insane
  // AI **INTERRUPTED**
  // File - CLOSE
  // Result: AITooSlow()


object App15 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      // TODO Handle inconsistency in this example
      // AI keeps timing out
      Scenario.DiskFull()
  // Network - Getting headline
  // Analytics - Scanning
  // File - OPEN
  // File - contains(genome)
  // Wiki - articleFor(genome)
  // AI - summarize - start
  // AI - summarize - end
  // File - disk full!
  // File - CLOSE
  // Result: DiskFull()


object App16 extends helpers.ZIOAppDebug:
  def run =
    researchHeadline:
      Scenario.StockMarketHeadline
  // Network - Getting headline
  // Analytics - Scanning
  // File - OPEN
  // File - contains(stock market)
  // Wiki - articleFor(stock market)
  // AI - summarize - start
  // AI - summarize - end
  // File - write: market is not rational
  // File - CLOSE
  // Result: market is not rational
