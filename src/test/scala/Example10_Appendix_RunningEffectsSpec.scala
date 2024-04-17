import zio.*
import zio.direct.*
import zio.test.*

object Example10_Appendix_RunningEffectsSpec extends ZIOSpecDefault:
  def spec = suite("suite"):
    test("random is random"):
      defer:
        assertTrue:
          Random.nextIntBounded(10).run < 10
    // spec50: ToTest[Nothing, Nothing] = mdoctools.ToTest@47d9a2e2
    // Result: Test PASSED
    + test("random is still random"):
      defer:
        assertTrue:
          Random.nextIntBetween(0, 10).run <= 10 &&
          Random.nextIntBetween(10, 20).run <= 20 &&
          Random.nextIntBetween(20, 30).run <= 30
    // spec56: ToTest[Nothing, Nothing] = mdoctools.ToTest@121476da
    // Result: Test PASSED
    + test("console works"):
      defer:
        TestConsole
          .feedLines:
            "Zeb"
          .run
    
        logic.run
    
        val capturedOutput: String =
          TestConsole.output.run.mkString
        val expectedOutput =
          s"""|Enter your name
              |Hello Zeb
              |""".stripMargin
        assertTrue:
          capturedOutput == expectedOutput
    // spec70: ToTest[Nothing, Nothing] = mdoctools.ToTest@3c83e4b
    // Result: Test PASSED
