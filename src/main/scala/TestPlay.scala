package com.ericriese.scwordle

import scala.io.Source

object TestPlay {
  def main(args: Array[String]): Unit =
    println(Guesses(Dictionary(Source.fromResource("words"))).next())
}
