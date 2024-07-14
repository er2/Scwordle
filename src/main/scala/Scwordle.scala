package com.ericriese.scwordle

import java.util.Scanner
import scala.io.Source

object Scwordle:

  /**
   * @param previousPlays in case of failure, Scwordle can be rerun with previous words to return to the middle of a game
   */
  def main(previousPlays: Array[String]): Unit =

    val guesses = Guesses(Source.fromResource("words").getLines().toSeq)

    val initialPlays = Plays(previousPlays.toList, guesses)

    var (firstPlay, plays) = initialPlays.next()
    println(firstPlay)

    var lastPlay = firstPlay
    var clue: Clue = KnowNothing

    val scanner = new Scanner(System.in)
    while (scanner.hasNext)
      val response = scanner.next()
      clue = clue + Clue.parse(lastPlay, response)
      val nextPlay = plays.next(new Filterer(clue))
      lastPlay = nextPlay._1
      plays = nextPlay._2
      println(lastPlay)
