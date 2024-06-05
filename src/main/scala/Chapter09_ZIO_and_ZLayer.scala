package Chapter09_ZIO_and_ZLayer

import zio.*
import zio.direct.*

def parse(
    contents: String
): ZIO[Any, IllegalArgumentException, Unit] =
  ???

def defaultGreeting()
    : ZIO[Any, Nothing, String] =
  ???