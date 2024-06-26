package Chapter02_Introduction

import zio.*
import zio.direct.*

def fp(a: Int, b: Int): Int = 
  a + b

val rand = new scala.util.Random

def fu(a: Int, b: Int): Int = 
  a + b + rand.nextInt()

val rand = new ControlledRandom

def fc(a: Int, b: Int): Int = 
  a + b + rand.nextInt()

def saveInformation(info: String): Unit =
  ???