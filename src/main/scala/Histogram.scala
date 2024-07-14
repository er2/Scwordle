package com.ericriese.scwordle

import scala.collection.immutable.TreeMap

class Histogram(words: Seq[String]):

  private val histo: Map[Char, Int] = words.flatten.groupMapReduce(k => k)(_ => 1)(_ + _)

  private val leastCommon = histo.values.minOption.getOrElse(1).toDouble

  private val uniqueRandoms = UniqueRandoms()

  private def scoreWord(word: String): Double =
    word.map(histo).map(_ / leastCommon).sum

  private def sortKey(word: String) =
    (-scoreWord(word), uniqueRandoms.next())

  def sort(words: Seq[String]): Iterable[String] =
    TreeMap.from(words.map(sortKey) zip words)
      .values