package Chapter08_State

import zio.*
import zio.direct.*

object App0 extends helpers.ZIOAppDebug:
  def run =
    var counter = 0
    val increment =
      ZIO.succeed:
        counter = counter + 1
  
    ZIO
      .foreachParDiscard(Range(0, 100_000)):
        _ => increment
      .as:
        s"Final count: $counter"
  // Result: Final count: 99839


object App1 extends helpers.ZIOAppDebug:
  def incrementCounter(count: Ref[Int]) =
    count.update:
      _ + 1
  
  def run =
    defer:
      val counter = Ref.make(0).run
      ZIO
        .foreachParDiscard(Range(0, 100_000)):
          _ => incrementCounter(counter)
        .run
      s"Final count: ${counter.get.run}"
  // Result: Final count: 100000


def expensiveCalculation =
  Thread.sleep:
    35

def sendNotification =
  println:
    "Alert: Updating Count!"

def updateAndNotify(count: Ref[Int]) =
  count.update:
    expensiveCalculation
    sendNotification
    _ + 1

object App2 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val counter = Ref.make(0).run
      ZIO
        .foreachParDiscard(Range(0, 4)):
          _ => updateAndNotify(counter)
        .run
      s"Final count: ${counter.get.run}"
  // TODO I'm not seeing the problem output here
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Result: Final count: 4


object App3 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val counter =
        Ref.Synchronized.make(0).run
  
      ZIO
        .foreachParDiscard(Range(0, 4)):
          _ => updateAndNotify(counter)
        .run
  
      s"Final count: ${counter.get.run}"
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Alert: Updating Count!
  // Result: Final count: 4
