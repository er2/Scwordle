package com.ericriese.scwordle

import scala.collection.immutable.TreeMap
import scala.util.Random

object Histogram:
  def apply(words: Seq[String]): Histogram =
    new Histogram(
      words.flatten.groupMapReduce(k => k)(_ => 1)(_ + _)
    )

class Histogram(histo: Map[Char, Int]):

  private val leastCommon = histo.values.minOption.getOrElse(1).toDouble

  private val random = new Random()

  private def scoreWord(word: String): Double =
    word.map(histo).map(_ / leastCommon).sum

  /**
   * Sort by
   * <li>negated score - to put the words with the most common letters at <code>head</code></li>
   * <li>randomness    - to keep games from being repetitive</li>
   * <li>word          - to make sure we don't lose the word to collisions in the treemap
   * in the unlikely case of words with the same score and random number
   * </li>
   */
  private def sortKey(word: String) =
    (-scoreWord(word), random.nextDouble(), word)

  def sort(words: Seq[String]): List[String] =
    TreeMap.from(words.map(sortKey) zip words)
      .values.toList
