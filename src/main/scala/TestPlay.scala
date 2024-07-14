package com.ericriese.scwordle

import scala.io.Source

object TestPlay:
  def main(args: Array[String]): Unit =
    println(Guesses(Source.fromResource("words").getLines().toSeq).pop())