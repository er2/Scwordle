package com.ericriese.scwordle

case class Clue(
                 positional: List[CharKnowledge],
                 notPresent: Set[Char],
                 somewheres: Set[Char]
               ) {
  def +(other: Clue): Clue = {
    val sumOfPositionals = combinePositionals(other.positional)
    val knownLetters = sumOfPositionals.collect {
      case Known(c) => c
    }.toSet
    val sumOfSomewheres = (this.somewheres ++ other.somewheres) -- knownLetters

    Clue(
      positional = sumOfPositionals,
      notPresent = this.notPresent ++ other.notPresent,
      somewheres = sumOfSomewheres
    )
  }

  private def combinePositionals(otherPositional: List[CharKnowledge]): List[CharKnowledge] = {
    (this.positional zip otherPositional).map {
      case (Known(a), Known(b)) if a != b => throw new Exception(s"determined to be both $a and $b")
      case (Known(c), _) => Known(c)
      case (_, Known(c)) => Known(c)
      case (Not(cs1: Set[Char]), Not(cs2: Set[Char])) => Not(cs1 ++ cs2)
      case (a, Unknown) => a
      case (Unknown, a) => a
    }
  }
}

object KnowNothing extends Clue(
  positional = List.fill(5)(Unknown),
  notPresent = Set(),
  somewheres = Set()
)

object Clue {

  /**
   * Response format
   * <pre>
   * v: 🟩 correct
   * x: ⬛ nowhere
   * ~: 🟨 somewhere else
   * </pre>
   */
  def parse(play: String, response: String): Clue = {
    response.ensuring(_.length == 5, "Response must be 5 characters long")
    (play zip response).zipWithIndex.map {
      case ((c, 'v'), i) => knowOneLetterAt(c, i)
      case ((c, 'x'), _) => no(c)
      case ((c, '~'), i) => elsewhere(c, i)
      case _ => throw new Exception("Invalid response format")
    }.reduce(_ + _)
  }

  private def knowOneLetterAt(c: Char, i: Int): Clue =
    Clue(
      positional = List.fill(5)(Unknown).updated(i, Known(c)),
      notPresent = Set(),
      somewheres = Set()
    )

  private def no(c: Char): Clue =
    Clue(
      positional = List.fill(5)(Unknown),
      notPresent = Set(c),
      somewheres = Set()
    )

  def elsewhere(c: Char, i: Int): Clue =
    Clue(
      positional = List.fill(5)(Unknown).updated(i, Not(Set(c))),
      notPresent = Set(),
      somewheres = Set(c)
    )
}
