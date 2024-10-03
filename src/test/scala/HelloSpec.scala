import zio.*
import zio.test.*
import zio.direct.*

// basic test
// testing effects
// simulated random
// controlled time

val billOrBruce =
  defer:
    if Random.nextBoolean.run then "Bill" else "Bruce"

val nightlyBatch =
  ZIO.sleep(24.hours)

object HelloSpec extends ZIOSpecDefault:
  def spec = test("hello"):
    assertTrue(1 == 1)

























    // assertTrue(ZIO.succeed(1) == 1)
//    defer:
//      val e = ZIO.succeed(1).run
//      assertTrue(e == 1)

//    defer:
//      nightlyBatch.zipPar(TestClock.adjust(24.hours)).run
//      assertCompletes

//    defer:
//      TestRandom.feedBooleans(true, false).run
//      val bill = billOrBruce.run
//      val bruce = billOrBruce.run
//      assertTrue(bill == "Bill" && bruce == "Bruce")
//  @@ TestAspect.repeat(Schedule.recurs(10))
