package com.ericriese.scwordle

import java.util.Scanner
import scala.io.Source

object Scwordle {

  /**
   * @param previousPlays in case of failure, Scwordle can be rerun with previous words to return to the middle of a game
   */
  def main(previousPlays: Array[String]): Unit = {

    val guesses = Guesses(Dictionary(Source.fromResource("words")))

    val candidates = previousPlays.iterator ++ Iterator.continually(guesses.next())

    val firstPlay = candidates.next()
    println(firstPlay)

    var lastPlay = firstPlay
    var clue: Clue = KnowNothing

    val scanner = new Scanner(System.in)
    while (scanner.hasNext) {
      val response = scanner.next()
      clue = clue + Clue.parse(lastPlay, response)
      guesses.filter(new Filterer(clue))
      lastPlay = candidates.next()
      println(lastPlay)
    }
  }

}
