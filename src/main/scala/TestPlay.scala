package com.ericriese.scwordle

import scala.io.Source
import scala.util.Random

object TestPlay:
  def main(args: Array[String]): Unit = {
    val seq = Source.fromResource("words").getLines().toSeq
    println(Random.shuffle(seq).head)
  }