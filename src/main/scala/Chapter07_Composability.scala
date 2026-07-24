package Chapter07_Composability

import zio.*
import zio.direct.*

import Scenario.*

var currentScenario: Scenario =
  DiskFull // D: D:

enum Scenario:
  case Successful
  case HeadlineError
  case BoringTopic
  case FileSystemError
  case WikiSystemError
  case AISlow
  case DiskFull

  def simulate[E, A](
      effect: ZIO[Scenario & Scope, E, A]
  ) =
    defer:
      ZIO
        .succeed:
          currentScenario = this
        .run
      ZIO
        .scoped(effect)
        .provide(ZLayer.succeed(this))
        .run
end Scenario

import scala.concurrent.Future
import Scenario.*

def getHeadline(): Future[String] =
  println("Network - Getting headline")
  currentScenario match
    case Scenario.HeadlineError =>
      Future.failed:
        new Exception(
          "Headline not available"
        )
    case Scenario.Successful =>
      Future
        .successful("stock market rising!")
    case Scenario.WikiSystemError =>
      Future.successful("Fred built a barn.")
    case Scenario.AISlow =>
      Future.successful("space is big!")
    case Scenario.FileSystemError =>
      Future
        .successful("new unicode released!")
    case Scenario.BoringTopic =>
      Future.successful("boring content")
    case Scenario.DiskFull =>
      Future
        .successful("human genome sequenced")
  end match
end getHeadline

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

case class NoWikiArticle()

def wikiArticle(
    topic: String
): Either[NoWikiArticle, String] =
  println(s"Wiki - articleFor($topic)")
  // simulates that this takes some time
  Thread.sleep(1_000)
  topic match
    case "stock market" | "space" |
        "genome" =>
      Right:
        s"detailed history of $topic"

    case "barn" =>
      Left:
        NoWikiArticle()

import scala.concurrent.Future

case class HeadlineNotAvailable()

val getHeadlineZ =
  ZIO
    .from:
      getHeadline()
    .orElseFail:
      HeadlineNotAvailable()

object App0 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      getHeadlineZ
  // Network - Getting headline
  // Result: stock market rising!


object App1 extends helpers.ZIOAppDebug:
  def run =
    HeadlineError.simulate:
      getHeadlineZ
  // Network - Getting headline
  // Error: HeadlineNotAvailable()


val result: Option[String] =
  findTopicOfInterest:
    "a boring headline"

case class NoInterestingTopic(
    headline: String
)

def topicOfInterestZ(headline: String) =
  ZIO
    .from:
      findTopicOfInterest:
        headline
    .orElseFail:
      NoInterestingTopic(headline)

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
      "boring and inane"
  // Analytics - Scanning for topic
  // Analytics - topic: None
  // Error: NoInterestingTopic(boring and inane)


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
  // Error: NoWikiArticle()


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
        case "file1" | "file2" | "file3" |
            "summaries" =>
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
        throw Exception(s"FileSystem error")
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

def openFileZ(path: String) =
  ZIO.fromAutoCloseable:
    ZIO.succeed:
      openFile(path)

object App6 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file = openFileZ("file1").run
      file.contains:
        "topicOfInterest"
  // File - OPEN: file1
  // File - contains(topicOfInterest) => false
  // File - CLOSE: file1
  // Result: false


object App7 extends helpers.ZIOAppDebug:
  import scala.util.Using
  
  def run =
    defer:
      Using(openFile("file1")):
        file1 =>
          Using(openFile("file2")):
            file2 =>
              Using(openFile("file3")):
                file3 =>
                  sameContents:
                    List(file1, file2, file3)
              .get
          .get
      .get
  // File - OPEN: file1
  // File - OPEN: file2
  // File - OPEN: file3
  // side-effect print: comparing content
  // File - CLOSE: file3
  // File - CLOSE: file2
  // File - CLOSE: file1
  // Result: true


object App8 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file1 = openFileZ("file1").run
      val file2 = openFileZ("file2").run
      val file3 = openFileZ("file3").run
      sameContents:
        List(file1, file2, file3)
  // File - OPEN: file1
  // File - OPEN: file2
  // File - OPEN: file3
  // side-effect print: comparing content
  // File - CLOSE: file3
  // File - CLOSE: file2
  // File - CLOSE: file1
  // Result: true


object App9 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val fileNames =
        List("file1", "file2", "file3")
  
      val files =
        ZIO.foreach(fileNames)(openFileZ).run
  
      sameContents(files)
  // File - OPEN: file1
  // File - OPEN: file2
  // File - OPEN: file3
  // side-effect print: comparing content
  // File - CLOSE: file3
  // File - CLOSE: file2
  // File - CLOSE: file1
  // Result: true


case class FileWriteFailure()

def writeToFileZ(
    file: File,
    content: String,
) =
  ZIO
    .from:
      file.write:
        content
    .orElseFail:
      FileWriteFailure()

object App10 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val file = openFileZ("file1").run
      writeToFileZ(file, "New data").run
  // File - OPEN: file1
  // File - write: New data
  // File - CLOSE: file1
  // Result: New data


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
  // Defect: java.lang.Exception: FileSystem error


case class FileReadFailure(topic: String)

def summaryForZ(file: File, topic: String) =
  ZIO
    .attempt:
      file.summaryFor(topic)
    .orElseFail:
      FileReadFailure(topic)

def summarize(article: String): String =
  println(s"AI - summarize - start")
  // Represents the AI taking a long time to
  // summarize the content
  if article.contains("space") then
    println("AI - taking a long time")
    Thread.sleep(5_000)

  println(s"AI - summarize - end")
  if article.contains("stock market") then
    s"market is moving"
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


import java.util.concurrent.TimeoutException

def summarizeZ(article: String) =
  ZIO
    .attemptBlockingInterrupt:
      summarize(article)
    .onInterrupt:
      ZIO.debug("AI **INTERRUPTED**")
    .timeoutFail(TimeoutException()):
      4.seconds

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
  // Error: java.util.concurrent.TimeoutException


val researchHeadline =
  defer:
    val headline: String = getHeadlineZ.run

    val topic: String =
      topicOfInterestZ(headline).run

    val summaryFile: File =
      openFileZ("summaries").run

    if summaryFile.contains(topic) then
      summaryForZ(summaryFile, topic).run
    else
      val wikiArticle: String =
        wikiArticleZ(topic).run

      val summary: String =
        summarizeZ(wikiArticle).run

      writeToFileZ(summaryFile, summary).run

      summary

object App16 extends helpers.ZIOAppDebug:
  def run =
    HeadlineError.simulate:
      researchHeadline
  // Network - Getting headline
  // Error: HeadlineNotAvailable()


object App17 extends helpers.ZIOAppDebug:
  def run =
    BoringTopic.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: None
  // Error: NoInterestingTopic(boring content)


object App18 extends helpers.ZIOAppDebug:
  def run =
    FileSystemError.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(unicode)
  // File - OPEN: summaries
  // File - contains(unicode) => true
  // File - summaryFor(unicode)
  // File - * Threw Exception *
  // File - CLOSE: summaries
  // Error: FileReadFailure(unicode)


object App19 extends helpers.ZIOAppDebug:
  def run =
    WikiSystemError.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(barn)
  // File - OPEN: summaries
  // File - contains(barn) => false
  // Wiki - articleFor(barn)
  // File - CLOSE: summaries
  // Error: NoWikiArticle()


object App20 extends helpers.ZIOAppDebug:
  def run =
    AISlow.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(space)
  // File - OPEN: summaries
  // File - contains(space) => false
  // Wiki - articleFor(space)
  // AI - summarize - start
  // AI - taking a long time
  // AI **INTERRUPTED**
  // File - CLOSE: summaries
  // Error: java.util.concurrent.TimeoutException


object App21 extends helpers.ZIOAppDebug:
  def run =
    DiskFull.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(genome)
  // File - OPEN: summaries
  // File - contains(genome) => false
  // Wiki - articleFor(genome)
  // AI - summarize - start
  // AI - summarize - end
  // File - disk full!
  // File - CLOSE: summaries
  // Error: FileWriteFailure()


object App22 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      researchHeadline
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(stock market)
  // File - OPEN: summaries
  // File - contains(stock market) => false
  // Wiki - articleFor(stock market)
  // AI - summarize - start
  // AI - summarize - end
  // File - write: market is moving
  // File - CLOSE: summaries
  // Result: market is moving


val quickResearch =
  researchHeadline
    .timeoutFail("strict timeout"):
      100.milliseconds

object App23 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      quickResearch
  // Network - Getting headline
  // Analytics - Scanning for topic
  // Analytics - topic: Some(stock market)
  // File - OPEN: summaries
  // File - contains(stock market) => false
  // Wiki - articleFor(stock market)
  // File - CLOSE: summaries
  // Error: strict timeout


val daily =
  Successful.simulate:
    quickResearch.repeat:
      Schedule.spaced(24.hours)