package com.ericriese.scwordle

import java.util.Scanner
import scala.io.Source
import scala.util.Random

object Scwordle {

  val random = new Random()

  def main(args: Array[String]): Unit = {
    val canonicalized = getDictionary
    var remainingCandidates = canonicalized
    val byDistinctLetterCount = canonicalized.groupBy(countLetters)
    val initialGuesses = byDistinctLetterCount(5)
    val recommendedFirstPlay = guess(initialGuesses)
    var lastPlay = recommendedFirstPlay
    println(recommendedFirstPlay)
    val scanner = new Scanner(System.in)
    while(scanner.hasNext) {
      val response = scanner.next()
      val guessResult = GuessResult.parse(lastPlay, response)
      remainingCandidates = remainingCandidates.filter(new Filterer(guessResult))
      lastPlay = guess(remainingCandidates)
      println("Recommended play: \n" + lastPlay)
    }
  }

  private def guess(candidates: Seq[String]): String = {
    if (candidates.isEmpty)
      throw new Error("out of ideas")
    candidates(random.nextInt(candidates.size))
  }

  private def countLetters(s: String) = s.toSet.size

  def getDictionary: Seq[String] = {
    val dictionary = Source.fromResource("words").getLines()
    val fives = dictionary
      .filter(!_.contains("'"))
      .filter(_.length == 5)
      .filter(_ (0).isLower)
    val canonicalized = removeAdjacentDuplicates(fives)
    canonicalized
  }

  private def removeAdjacentDuplicates(in: Iterator[String]): Seq[String] = {
    in.foldLeft(List[String]())((list, word) => if (word.equalsIgnoreCase(list.headOption.getOrElse(""))) list else word :: list)
  }
}
