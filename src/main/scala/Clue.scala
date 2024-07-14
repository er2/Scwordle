package com.ericriese.scwordle

import scala.annotation.targetName

case class Clue(
                 positional: List[CharKnowledge],
                 somewheres: Set[Char]
               ):
  @targetName("plus")
  def +(other: Clue): Clue =
    val sumOfPositionals = combinePositionals(other.positional)
    val knownLetters = sumOfPositionals.collect {
      case Known(c) => c
    }.toSet
    val sumOfSomewheres = (this.somewheres ++ other.somewheres) -- knownLetters
    Clue(
      positional = sumOfPositionals,
      somewheres = sumOfSomewheres
    )

  private def combinePositionals(otherPositional: List[CharKnowledge]): List[CharKnowledge] =
    (this.positional zip otherPositional).map {
      case (Known(a), Known(b)) if a != b => throw new Exception(s"determined to be both $a and $b")
      case (Known(c), _) => Known(c)
      case (_, Known(c)) => Known(c)
      case (Not(cs1: Set[Char]), Not(cs2: Set[Char])) => Not(cs1 ++ cs2)
      case (a, Unknown) => a
      case (Unknown, a) => a
    }

object KnowNothing extends Clue(
  positional = List.fill(5)(Unknown),
  somewheres = Set()
)

object Clue:

  /**
   * Response format
   * <pre>
   * v: 🟩 correct
   * x: ⬛ nowhere
   * ~: 🟨 somewhere else
   * ?: previous suggestion not in Wordle dictionary
   * </pre>
   */
  def parse(play: String, response: String): Clue =
    if (response.contains("?"))
      return KnowNothing
    response.ensuring(_.length == 5, "Response must be 5 characters long")
    (play zip response).zipWithIndex.map {
      case ((c, 'v'), i) => knowOneLetterAt(c, i)
      case ((c, 'x'), _) => no(c)
      case ((c, '~'), i) => elsewhere(c, i)
      case _ => throw new Exception("Invalid response format")
    }.reduce(_ + _)

  private def knowOneLetterAt(c: Char, i: Int): Clue =
    Clue(
      positional = List.fill(5)(Unknown).updated(i, Known(c)),
      somewheres = Set()
    )

  private def no(c: Char): Clue =
    Clue(
      positional = List.fill(5)(Not(Set(c))),
      somewheres = Set()
    )

  private def elsewhere(c: Char, i: Int): Clue =
    Clue(
      positional = List.fill(5)(Unknown).updated(i, Not(Set(c))),
      somewheres = Set(c)
    )
