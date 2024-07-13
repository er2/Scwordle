package com.ericriese.scwordle

import Guesses.countLetters

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

  def countLetters(s: String): Int = s.toSet.size

class Guesses(map: mutable.TreeMap[Int, mutable.Stack[String]]):

  def filter(filter: String => Boolean): Unit =
    map.values.foreach(_.removeAll(s => !filter(s)))
    removeEmptyEntries()

  def remove(s: String): Unit =
    map(countLetters(s)).removeAll(_ == s)
    removeEmptyEntries()

  private def removeEmptyEntries(): Unit =
    map.filterInPlace((_, v) => v.nonEmpty)

  def next(): String =
    val result = map.lastOption.get._2.pop()
    removeEmptyEntries()
    result

  def isEmpty: Boolean = map.isEmpty