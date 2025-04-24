package helpers_kyo

import kyo.*

import scala.reflect.ClassTag

//case class Counter(var current: Int)

import AllowUnsafe.embrace.danger

enum Scenario:
  case HappyPath
  case NeverWorks
  case Slowly
  case WorksOnTry(attempts: Int, state: AtomicInt = IO.Unsafe.evalOrThrow(AtomicInt.init(0)))
  // "injects" the scenario and a counter into the Effect
  def apply[A: Flat, E >: Nothing, S](effect: A < (Env[Scenario] & Abort[E] & S))(using SafeClassTag[E], Tag[E]): Result[E, A] < S =
    Env.run(this):
      Abort.run(effect)

object Scenario:
  def Slow[A: Flat, S](effect: A < (Env[Scenario] & Abort[String | Timeout] & S)): Result[String, A] < S =
    Env.run(Scenario.Slowly):
      Abort.run:
        effect.forAbort[Timeout].mapAbort(_ => Abort.fail("Timed out"))

def saveUser(username: String): String < (Env[Scenario] & IO & Abort[String] & Async) =
  defer:
    Console.printLine(s"Attempting to save $username").now
    Env.get[Scenario].now match
      case Scenario.HappyPath =>
        IO("User saved").now
      case Scenario.NeverWorks =>
        Abort.fail("User not saved").now
      case Scenario.Slowly =>
        Kyo.sleep(5.seconds).now
        IO("User saved").now
      case Scenario.WorksOnTry(attempts, counter) =>
        val count = counter.incrementAndGet.now
        if count < attempts then
          Abort.fail("User not saved").now
        else
          IO("User saved").now
