package Chapter02_Introduction

import zio.*
import zio.direct.*

// TODO This section might get a bit too detailed. Some of the content could move to the Failure chapter to keep this tighter/introductory. It might also be fine as is, just want to discuss.

val x = 2
// I would say no. We are just trying to describe the big picture here, and hello world
// Doesn't add to that, and possibly confuses things.
// TODO We mention hello world, should we show vanilla hello vs ZIO hello? First draft below:

object App0 extends helpers.ZIOAppDebug:
  def run =
    ZIO.debug("hello world")
  // hello world
