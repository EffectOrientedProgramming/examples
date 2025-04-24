import zio.*
import zio.test.*
import zio.direct.*


// basic test
object HelloSpec extends ZIOSpecDefault:
  def spec = test("hello"):
    assertTrue(1 == 1)



// testing effects
object EffectBasicSpec extends ZIOSpecDefault:
  def spec = test("hello"):
    // assertTrue(ZIO.succeed(1) == 1)
    defer:
      val e = ZIO.succeed(2).run
      assertTrue(e == 1)



// simulated random
val billOrBruce =
  defer:
    if Random.nextBoolean.run then "Bill" else "Bruce"

object EffectRandomSpec extends ZIOSpecDefault:
  def spec = test("hello"):
    defer:
      TestRandom.feedBooleans(true, false).run
      val bill = billOrBruce.run
      val bruce = billOrBruce.run
      assertTrue(bill == "Bill" && bruce == "Bruce")
  @@ TestAspect.repeat(Schedule.recurs(10))



// controlled time
val nightlyBatch =
  ZIO.sleep(24.hours)

object EffectTimeSpec extends ZIOSpecDefault:
  def spec = test("hello"):
    defer:
      nightlyBatch.zipPar(TestClock.adjust(24.hours)).run
      assertCompletes
