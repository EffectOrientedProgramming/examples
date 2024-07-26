package Chapter07_Composability

import zio.*
import zio.direct.*
import zio.Console.*

enum Scenario:
  case StockMarketHeadline
  case HeadlineNotAvailable
  case NoInterestingTopic()
  // There is an
  // Either[NoWikiArticleAvailable,_]
  // in visible code, so if we make it an
  // object,
  // It will be
  // Either[NoWikiArticleAvailable.type,_] :(
  case NoWikiArticleAvailable()
  case AITooSlow()
  case SummaryReadThrows()
  case DiskFull()

import Scenario.*

// the scenario is used from non-ZIO code, so we don't use the config / bootstrap approach to passing it.
// but we do still use bootstrap to set the scenario, just for consistency with how the scenario is set in other chapters
var scenario: Scenario = StockMarketHeadline

def stockMarketHeadline =
  scenario = StockMarketHeadline
  ZLayer.empty

def headlineNotAvailable =
  scenario = HeadlineNotAvailable
  ZLayer.empty

def noInterestingTopic =
  scenario = NoInterestingTopic()
  ZLayer.empty

def summaryReadThrows =
  scenario = SummaryReadThrows()
  ZLayer.empty

def noWikiArticleAvailable =
  scenario = NoWikiArticleAvailable()
  ZLayer.empty

def aiTooSlow =
  scenario = AITooSlow()
  ZLayer.empty

def diskFull =
  scenario = DiskFull()
  ZLayer.empty

import scala.concurrent.Future
def getHeadLine(): Future[String] =
  println("Network - Getting headline")
  scenario match
    case Scenario.HeadlineNotAvailable =>
      Future.failed:
        new Exception(
          "Headline not available"
        )
    case Scenario.StockMarketHeadline =>
      Future
        .successful("stock market rising!")
    case Scenario.NoWikiArticleAvailable() =>
      Future.successful("Fred built a barn.")
    case Scenario.AITooSlow() =>
      Future.successful("space is big!")
    case Scenario.SummaryReadThrows() =>
      Future
        .successful("new unicode released!")
    case Scenario.NoInterestingTopic() =>
      Future.successful("boring content")
    case Scenario.DiskFull() =>
      Future
        .successful("human genome sequenced")
  end match
end getHeadLine

def findTopicOfInterest(
    content: String
): Option[String] =
  println("Analytics - Scanning for topic")
  val topics =
    List(
      "stock market",
      "space",
      "barn",
      "unicode",
      "genome",
    )
  val res = topics.find(content.contains)
  println(s"Analytics - topic: $res")
  res

import scala.util.Either
def wikiArticle(topic: String): Either[
  Scenario.NoWikiArticleAvailable,
  String,
] =
  println(s"Wiki - articleFor($topic)")
  topic match
    case "stock market" | "space" |
        "genome" =>
      Right:
        s"detailed history of $topic"

    case "barn" =>
      Left:
        Scenario.NoWikiArticleAvailable()

import scala.concurrent.Future

def getHeadlineZ() =
  ZIO
    .from:
      getHeadLine()
    .orElseFail:
      HeadlineNotAvailable

object App0 extends helpers.ZIOAppDebug:
  override val bootstrap = stockMarketHeadline
  
  def run =
    getHeadlineZ()
  // Network - Getting headline
  // Result: stock market rising!


object App1 extends helpers.ZIOAppDebug:
  override val bootstrap = headlineNotAvailable
  
  def run =
    getHeadlineZ()
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
      NoInterestingTopic()

object App2 extends helpers.ZIOAppDebug:
  def run =
    topicOfInterestZ:
      "stock market rising!"
  // Analytics - Scanning for topic
  // Analytics - topic: Some(stock market)
  // Result: stock market


object App3 extends helpers.ZIOAppDebug:
  def run =
    topicOfInterestZ:
      "boring and inane content"
  // Analytics - Scanning for topic
  // Analytics - topic: None
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

trait File extends AutoCloseable:
  def contains(searchTerm: String): Boolean
  def write(entry: String): Try[String]
  def summaryFor(searchTerm: String): String
  def content(): String

def sameContents(
    files: List[File]
): Boolean =
  println:
    "side-effect print: comparing content"

  files
    .tail
    .forall(
      _.content() == files.head.content()
    )

def openFile(path: String) =
  new File:
    var contents: List[String] =
      List("Medical Breakthrough!")
    println(s"File - OPEN: $path")

    override def content() =
      path match
        case "file1.txt" | "file2.txt" |
            "file3.txt" | "summaries.txt" =>
          "hot dog"
        case _ =>
          "not hot dog"

    override def close =
      println(s"File - CLOSE: $path")

    override def contains(
        searchTerm: String
    ): Boolean =

      val result =
        searchTerm match
          case "wheel" | "unicode" =>
            true
          case _ =>
            false
      println:
        s"File - contains($searchTerm) => $result"
      result

    override def summaryFor(
        searchTerm: String
    ): String =
      println(
        s"File - summaryFor($searchTerm)"
      )
      if searchTerm == "unicode" then
        println("File - * Threw Exception *")
        throw Exception(s"No summary found")
      else if searchTerm == "stock market"
      then
        "stock markets are neat"
      else if searchTerm == "space" then
        "space is huge"
      else
        ???

    override def write(
        entry: String
    ): Try[String] =
      if entry.contains("genome") then
        println("File - disk full!")
        Try(
          throw new Exception(
            "Disk is full!"
          )
        )
      else
        println("File - write: " + entry)
        contents = entry :: contents
        Try(entry)

// TODO explain that fromAutoClosable takes a ZIO, thus the ZIO.succeed
def openFileZ(path: String) =
  ZIO.fromAutoCloseable:
    ZIO.succeed:
      openFile(path)

object App6 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file = openFileZ("file1.txt").run
      file.contains:
        "topicOfInterest"
  // File - OPEN: file1.txt
  // File - contains(topicOfInterest) => false
  // File - CLOSE: file1.txt
  // Result: false


object App7 extends helpers.ZIOAppDebug:
  import scala.util.Using
  
  def run =
    defer:
      Using(openFile("file1.txt")):
        file1 =>
          Using(openFile("file2.txt")):
            file2 =>
              Using(openFile("file3.txt")):
                file3 =>
                  sameContents:
                    List(file1, file2, file3)
              .get
          .get
      .get
  // File - OPEN: file1.txt
  // File - OPEN: file2.txt
  // File - OPEN: file3.txt
  // side-effect print: comparing content
  // File - CLOSE: file3.txt
  // File - CLOSE: file2.txt
  // File - CLOSE: file1.txt
  // Result: true


object App8 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file1 = openFileZ("file1.txt").run
      val file2 = openFileZ("file2.txt").run
      val file3 = openFileZ("file3.txt").run
      sameContents:
        List(file1, file2, file3)
  // File - OPEN: file1.txt
  // File - OPEN: file2.txt
  // File - OPEN: file3.txt
  // side-effect print: comparing content
  // File - CLOSE: file3.txt
  // File - CLOSE: file2.txt
  // File - CLOSE: file1.txt
  // Result: true


object App9 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val fileNames =
        List(
          "file1.txt",
          "file2.txt",
          "file3.txt",
        )
  
      val files =
        ZIO.foreach(fileNames)(openFileZ).run
  
      sameContents(files)
  // File - OPEN: file1.txt
  // File - OPEN: file2.txt
  // File - OPEN: file3.txt
  // side-effect print: comparing content
  // File - CLOSE: file3.txt
  // File - CLOSE: file2.txt
  // File - CLOSE: file1.txt
  // Result: true


def writeToFileZ(
    file: File,
    content: String,
) =
  ZIO
    .from:
      file.write:
        content
    .orElseFail:
      DiskFull()

object App10 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file = openFileZ("file1").run
      writeToFileZ(file, "New data on topic")
        .run
  // File - OPEN: file1
  // File - write: New data on topic
  // File - CLOSE: file1
  // Result: New data on topic


object App11 extends helpers.ZIOAppDebug:
  def run =
    defer:
      openFile("file1").summaryFor("space")
  // File - OPEN: file1
  // File - summaryFor(space)
  // Result: space is huge


object App12 extends helpers.ZIOAppDebug:
  def run =
    defer:
      openFile("file1").summaryFor("unicode")
  // File - OPEN: file1
  // File - summaryFor(unicode)
  // File - * Threw Exception *
  // Result: Defect: java.lang.Exception: No summary found


class NoSummaryAvailable(topic: String)

def summaryForZ(file: File, topic: String) =
  ZIO
    .attempt:
      file.summaryFor(topic)
    .orElseFail:
      NoSummaryAvailable(topic)

def summarize(article: String): String =
  println(s"AI - summarize - start")
  // Represents the AI taking a long time to
  // summarize the content
  if article.contains("space") then
    println("AI - taking a long time")
    Thread.sleep(5_000)

  println(s"AI - summarize - end")
  if article.contains("stock market") then
    s"market is not rational"
  else if article.contains("genome") then
    "The human genome is huge!"
  else if article.contains("long article")
  then
    "short summary"
  else
    ???

object App13 extends helpers.ZIOAppDebug:
  def run =
    defer:
      summarize("long article")
  // AI - summarize - start
  // AI - summarize - end
  // Result: short summary


def summarizeZ(article: String) =
  ZIO
    .attemptBlockingInterrupt:
      summarize(article)
    .orDie
    .onInterrupt:
      ZIO.debug("AI **INTERRUPTED**")
    .timeoutFail(AITooSlow()):
      4_000.millis

object App14 extends helpers.ZIOAppDebug:
  def run =
    summarizeZ("long article")
  // AI - summarize - start
  // AI - summarize - end
  // Result: short summary


object App15 extends helpers.ZIOAppDebug:
  def run =
    summarizeZ("space")
  // AI - summarize - start
  // AI - taking a long time
  // AI **INTERRUPTED**
  // Result: AITooSlow()


val researchHeadline =
  defer:
    val headline: String = getHeadlineZ().run

    val topic: String =
      topicOfInterestZ(headline).run

    val summaryFile: File =
      openFileZ("summaries.txt").run

    val knownTopic: Boolean =
      summaryFile.contains:
        topic

    if knownTopic then
      summaryForZ(summaryFile, topic).run
    else
      val wikiArticle: String =
        wikiArticleZ(topic).run

      val summary: String =
        summarizeZ(wikiArticle).run

      writeToFileZ(summaryFile, summary).run
      summary

object App16 extends helpers.ZIOAppDebug:
  override val bootstrap = headlineNotAvailable
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Result: HeadlineNotAvailable


object App17 extends helpers.ZIOAppDebug:
  override val bootstrap = noInterestingTopic
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: None
  // Result: NoInterestingTopic()


object App18 extends helpers.ZIOAppDebug:
  override val bootstrap = summaryReadThrows
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(unicode)
  // File - OPEN: summaries.txt
  // File - contains(unicode) => true
  // File - summaryFor(unicode)
  // File - * Threw Exception *
  // File - CLOSE: summaries.txt
  // Result: repl.MdocSession$MdocApp$NoSummaryAvail


object App19 extends helpers.ZIOAppDebug:
  override val bootstrap =
    noWikiArticleAvailable
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(barn)
  // File - OPEN: summaries.txt
  // File - contains(barn) => false
  // Wiki - articleFor(barn)
  // File - CLOSE: summaries.txt
  // Result: NoWikiArticleAvailable()


object App20 extends helpers.ZIOAppDebug:
  override val bootstrap = aiTooSlow
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(space)
  // File - OPEN: summaries.txt
  // File - contains(space) => false
  // Wiki - articleFor(space)
  // AI - summarize - start
  // AI - taking a long time
  // AI **INTERRUPTED**
  // File - CLOSE: summaries.txt
  // Result: AITooSlow()


object App21 extends helpers.ZIOAppDebug:
  override val bootstrap = diskFull
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(genome)
  // File - OPEN: summaries.txt
  // File - contains(genome) => false
  // Wiki - articleFor(genome)
  // AI - summarize - start
  // AI - summarize - end
  // File - disk full!
  // File - CLOSE: summaries.txt
  // Result: DiskFull()


object App22 extends helpers.ZIOAppDebug:
  override val bootstrap = stockMarketHeadline
  
  def run =
    researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(stock market)
  // File - OPEN: summaries.txt
  // File - contains(stock market) => false
  // Wiki - articleFor(stock market)
  // AI - summarize - start
  // AI - summarize - end
  // File - write: market is not rational
  // File - CLOSE: summaries.txt
  // Result: market is not rational


val strictResearch =
  researchHeadline
    .timeoutFail("strict timeout"):
      1.microsecond

object App23 extends helpers.ZIOAppDebug:
  override val bootstrap = stockMarketHeadline
  
  def run =
    strictResearch
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(stock market)
  // File - OPEN: summaries.txt
  // File - contains(stock market) => false
  // Wiki - articleFor(stock market)
  // AI - summarize - start
  // AI - summarize - end
  // File - CLOSE: summaries.txt
  // Result: strict timeout


val daily =
  strictResearch.repeat:
    Schedule.spaced(24.hours)