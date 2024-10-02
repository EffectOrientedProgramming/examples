package Chapter08_Shared_State

import zio.*
import zio.direct.*

import java.util.concurrent.atomic.AtomicInteger

def parallel(
    num: Int
)(effect: ZIO[Any, Nothing, Unit]) =
  ZIO
    .foreachPar(Range(0, num)):
      _ => effect
    .unit

class AttemptCounter extends AtomicInteger:
  def increment =
    incrementAndGet()

object App0 extends helpers.ZIOAppDebug:
  def run =
    val num     = 30_000
    var counter = 0
    val increment =
      ZIO.succeed:
        counter = counter + 1
  
    parallel(num):
      increment
    .as:
      s"Lost updates: ${num - counter}"
  // Result: Lost updates: 1


object App1 extends helpers.ZIOAppDebug:
  def increment(count: Ref[Int]) =
    count.update:
      value => value + 1
  
  def run =
    defer:
      val counter = Ref.make(0).run
      parallel(30_000):
        increment(counter)
      .run
      s"counter: ${counter.get.run}"
  // Result: counter: 30000


object App2 extends helpers.ZIOAppDebug:
  val attempts = AttemptCounter()
  
  def increment(count: Ref[Int]) =
    count.update:
      value =>
        attempts.increment
        value + 1
  
  def run =
    defer:
      val counter = Ref.make(0).run
      parallel(30_000):
        increment(counter)
      .run
  
      s"""
counter: ${counter.get.run}
attempts: ${attempts.get}
""".stripMargin
  // Result: 
  // counter: 30000
  // attempts: 30042


object App3 extends helpers.ZIOAppDebug:
  val attempts = AttemptCounter()
  
  def increment(count: Ref.Synchronized[Int]) =
    count.updateZIO:
      value =>
        ZIO.succeed:
          attempts.increment
          value + 1
  
  def run =
    defer:
      val counter =
        Ref.Synchronized.make(0).run
      parallel(30_000):
        increment(counter)
      .run
  
      s"""
counter: ${counter.get.run}
attempts: ${attempts.get}
""".stripMargin
  // Result: 
  // counter: 30000
  // attempts: 30000
