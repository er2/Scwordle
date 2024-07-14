package com.ericriese.scwordle

import scala.collection.immutable.TreeMap

object Guesses:
  def apply(words: Seq[String]): Guesses =
    val histogram = Histogram(words)
    new Guesses(
      TreeMap.from(
        words.groupBy(countLetters)
          .view.mapValues(histogram.sort)
      )
    )

  private def countLetters(s: String): Int = s.toSet.size

class Guesses(map: TreeMap[Int, List[String]]) extends PlaySource:

  def filter(filter: String => Boolean): Guesses =
    new Guesses(
      TreeMap.from(
        map.view.mapValues(_.filter(w => filter(w)))
          .filter((letterCount, words) => words.nonEmpty)
      )
    )

  override def next(filterer: String => Boolean): (String, PlaySource) =
    filter(filterer).next()

  override def next(): (String, Guesses) =
    val (count, words) = map.last
    words match
      case head::tail => (words.head, new Guesses(map.updated(count, tail)))
      case Nil => new Guesses(map.removed(count)).next()

  export map.isEmpty

