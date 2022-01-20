package com.ericriese.scwordle

import java.util.Scanner
import scala.io.Source
import scala.util.Random

object Scwordle {

  val random = new Random()

  def main(args: Array[String]): Unit = {

    var remainingCandidates = Dictionary(Source.fromResource("words"))

    val firstPlay = {
      val initialGuesses = remainingCandidates.filter(countLetters(_) == 5)
      pick(initialGuesses)
    }
    println(firstPlay)

    var lastPlay = firstPlay
    var clue: Clue = KnowNothing

    val scanner = new Scanner(System.in)
    while (scanner.hasNext) {
      val response = scanner.next()
      clue = clue + Clue.parse(lastPlay, response)
      remainingCandidates = remainingCandidates.filter(new Filterer(clue))
      lastPlay = pick(remainingCandidates)
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
