import zio.*
import zio.direct.*

trait ZIO[R, E, A]

def parse(
    contents: String
): ZIO[Any, IllegalArgumentException, Unit] =
  ???

def defaultGreeting()
    : ZIO[Any, Nothing, String] =
  ???