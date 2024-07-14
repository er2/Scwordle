package com.ericriese.scwordle

import scala.collection.mutable
import scala.util.Random

object Guesses:
  def apply(words: Seq[String]): Guesses =
    new Guesses(
      mutable.TreeMap.from(
        words.groupBy(countLetters)
          .view.mapValues(shuffle)
      )
    )

  private def shuffle(s: Seq[String]): mutable.Stack[String] =
    mutable.Stack.from(Random.shuffle(s))

  private def countLetters(s: String): Int = s.toSet.size

class Guesses(map: mutable.TreeMap[Int, mutable.Stack[String]]):

  def filter(filter: String => Boolean): Unit =
    map.values.foreach(_.removeAll(s => !filter(s)))
    map.filterInPlace((_, words) => words.nonEmpty)

  def pop(): String =
    val (count, words) = map.last
    val result = words.pop()
    if (words.isEmpty)
      map.remove(count)
    result

  export map.isEmpty