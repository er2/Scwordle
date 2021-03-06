package com.ericriese.scwordle

import java.util.Scanner
import scala.::
import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Scwordle {

  val random = new Random()

  /**
   * @param previousPlays in case of failure, Scwordle can be rerun with previous words to return to the middle of a game
   */
  def main(previousPlays: Array[String]): Unit = {

    var remainingCandidates = Dictionary(Source.fromResource("words"))

    val candidates = {
      val firstPlays = if (previousPlays.nonEmpty) {
        previousPlays.iterator
      } else {
        val initialGuesses = remainingCandidates.filter(countLetters(_) == 5)
        val firstPlay = pick(initialGuesses)
        Iterator.single(firstPlay)
      }
      firstPlays ++ Iterator.continually(pick(remainingCandidates))
    }

    val firstPlay = candidates.next()
    println(firstPlay)

    var lastPlay = firstPlay
    var clue: Clue = KnowNothing

    val scanner = new Scanner(System.in)
    while (scanner.hasNext) {
      val response = scanner.next()
      clue = clue + Clue.parse(lastPlay, response)
      remainingCandidates = remainingCandidates.filter(new Filterer(clue))
      lastPlay = candidates.next()
      println(lastPlay)
    }
  }

  private def pick(candidates: Seq[String]): String = {
    if (candidates.isEmpty)
      throw new Exception("out of ideas")
    candidates(random.nextInt(candidates.size))
  }

  private def countLetters(s: String) = s.toSet.size

}
