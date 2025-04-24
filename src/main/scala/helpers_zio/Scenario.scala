package helpers_zio

import zio.*
import zio.direct.*

// Enables us to add a debug to print the output of the program
trait ZIOAppDebug:
  self =>

  type Environment = Any

  val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = ZLayer.empty

  val environmentTag: EnvironmentTag[Any] = EnvironmentTag[Any]

  def run: ZIO[ZIOAppArgs & Scope, Any, Any]

  def main(args: Array[String]): Unit =
    val app = new ZIOAppDefault:
      override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
        self.bootstrap

      override def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
        // this tries to create consistency for how we run and print output between the book and the examples
        ZIO.scoped:
          self.run
        .tapSome:
          case result if !result.isInstanceOf[Unit] =>
            Console.printLine:
              s"Result: $result"

    app.main(args)


enum Scenario:
  case Successful
  case NeverWorks
  case Slow
  case WorksOnTryInner(ref: Ref[Int])

  def simulate[E, A](effect: ZIO[Scenario & Scope, E, A]) =
    defer:
      ZIO
        .succeed:
          currentScenario = this
        .run
      ZIO
        .scoped(effect)
        .provide(ZLayer.succeed(this))
        .run

object Scenario:
  // A bit of trickery here, so that the
  // reader thinks they're seeing
  // just-another-enum case, even though it's
  // calling some unsafe stuff behind the
  // scenes to create the *real* enum case

  def WorksOnThirdTry: WorksOnTryInner =
    Unsafe.unsafe:
      implicit unsafe =>
        WorksOnTryInner(
          Runtime
            .default
            .unsafe
            .run(Ref.make(0))
            .getOrThrow()
        )

def saveUser(username: String) =
  val succeed =
    ZIO.succeed:
      "User saved"

  val fail =
    ZIO
      .fail:
        "**Database crashed!!**"
      .tapError:
        error =>
          ZIO.debug:
            "Log: " + error

  val logic =
    defer:
      val scenario =
        ZIO.service[Scenario].run
      ZIO
        .debug("Attempting to save user")
        .run
      scenario match
        case Scenario.NeverWorks =>
          fail.run

        case Scenario.Slow =>
          ZIO.sleep(1.minute).run
          succeed.run

        case Scenario.WorksOnTryInner(ref) =>
          val numCalls =
            ref.getAndUpdate(_ + 1).run
          if numCalls == 2 then
            succeed.run
          else
            fail.run

        case Scenario.Successful =>
          succeed.run
  logic
end saveUser

def sendToManualQueue(username: String) =
  ZIO.attempt:
    s"Sent $username to manual queue"

def logUserSignup(username: String) =
  ZIO.debug:
    s"Log: Signup initiated for $username"

var currentScenario: Scenario = Scenario.NeverWorks
