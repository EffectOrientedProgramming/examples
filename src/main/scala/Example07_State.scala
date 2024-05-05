import zio.*
import zio.direct.*

val unreliableCounting =
  var counter =
    0
  val increment =
    ZIO.succeed:
      counter =
        counter + 1

  defer:
    ZIO
      .foreachParDiscard(Range(0, 100000)):
        _ => increment
      .run
    // It's not obvious to the reader why
    // we need to wrap counter in .succeed
    "Final count: " + ZIO.succeed(counter).run

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

val sideEffectingUpdatesSync =
  defer:
    val counter =
      Ref.Synchronized.make(0).run
    ZIO
      .foreachParDiscard(Range(0, 4)):
        _ => update(counter)
      .run
    val finalCount =
      counter.get.run
    s"Final count: $finalCount"

object Example07_State_0 extends ZIOAppDefault:
  def run =
    unreliableCounting
  // Result: Final count: 99577


object Example07_State_1 extends ZIOAppDefault:
  lazy val reliableCounting =
    def incrementCounter(counter: Ref[Int]) =
      counter.update:
        _ + 1
  
    defer:
      val counter =
        Ref.make(0).run
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


object Example07_State_2 extends ZIOAppDefault:
  def run =
    defer:
      val counter =
        Ref.make(0).run
      ZIO
        .foreachParDiscard(Range(0, 4)):
          _ => update(counter)
        .run
      val finalCount =
        counter.get.run
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


object Example07_State_3 extends ZIOAppDefault:
  def run =
    sideEffectingUpdatesSync
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Alert: updating count!
  // Result: Final count: 4
