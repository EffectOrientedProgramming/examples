package Chapter08_State

import zio.*
import zio.direct.*
import zio.Console.*

val unreliableCounting =
  var counter = 0
  val increment =
    ZIO.succeed:
      counter = counter + 1

  defer:
    ZIO
      .foreachParDiscard(Range(0, 100000)):
        _ => increment
      .run
    // It's not obvious to the reader why
    // we must wrap counter in .succeed
    "Final count: " +
      ZIO.succeed(counter).run

object App0 extends helpers.ZIOAppDebug:
  def run =
    unreliableCounting
  // Result: Final count: 99992


object App1 extends helpers.ZIOAppDebug:
  lazy val reliableCounting =
    def incrementCounter(counter: Ref[Int]) =
      counter.update:
        _ + 1
  
    defer:
      val counter = Ref.make(0).run
      ZIO
        .foreachParDiscard(Range(0, 100000)):
          _ =>
            incrementCounter:
              counter
        .run
      "Final count: " + counter.get.run
  
  def run =
    reliableCounting
  // Result: Final count: 100000


def expensiveCalculation() =
  Thread.sleep:
    35

def sendNotification() =
  println:
    "Alert: updating count!"

def update(counter: Ref[Int]) =
  counter.update:
    previousValue =>
      expensiveCalculation()
      sendNotification()
      previousValue + 1

object App2 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val counter = Ref.make(0).run
      ZIO
        .foreachParDiscard(Range(0, 4)):
          _ => update(counter)
        .run
      val finalCount = counter.get.run
      s"Final count: $finalCount"
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Result: Final count: 4


val sideEffectingUpdatesSync =
  defer:
    val counter =
      Ref.Synchronized.make(0).run
    ZIO
      .foreachParDiscard(Range(0, 4)):
        _ => update(counter)
      .run
    val finalCount = counter.get.run
    s"Final count: $finalCount"

object App3 extends helpers.ZIOAppDebug:
  def run =
    sideEffectingUpdatesSync
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Result: Final count: 4
