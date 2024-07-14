package com.ericriese.scwordle

class Histogram(words: Seq[String]):

  private val histo: Map[Char, Int] = words.flatten.groupMapReduce(k => k)(_ => 1)(_ + _)

  private val leastCommon = histo.values.minOption.getOrElse(1).toDouble

  private def scoreWord(word: String): Double =
    word.map(histo).map(_ / leastCommon).sum

  def sort(words: Seq[String]): List[String] =
    words.zip(words.map(scoreWord)).sortBy {
      case (word, score) => (-score, Math.random())
    }.map(_._1).toList