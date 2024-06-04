package Chapter09_ZIO_and_ZLayer

import zio.*
import zio.direct.*

trait ZIO[Requirements, Error, Answer]

trait ZIO[R, E, A]

def parse(
    contents: String
): ZIO[Any, IllegalArgumentException, Unit] =
  ???

def defaultGreeting()
    : ZIO[Any, Nothing, String] =
  ???