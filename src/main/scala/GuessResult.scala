package com.ericriese.scwordle

case class GuessResult(
                        positional: List[CharKnowledge],
                        knownAbsent: Set[Char],
                        somewhere: Set[Char]
                      ) {
  def +(other: GuessResult): GuessResult = {
    val sumOfPositionals: List[CharKnowledge] = (this.positional zip other.positional).map {
      case (Known(c), _) => Known(c)
      case (_, Known(c)) => Known(c)
      case (Known(a), Known(b)) if a != b => throw new Exception(s"determined to be both $a and $b")
      case (Not(cs1: Set[Char]), Not(cs2: Set[Char])) => Not(cs1 ++ cs2)
      case (a, Unknown) => a
      case (Unknown, a) => a
    }
    val knownLetters = sumOfPositionals.collect {
      case Known(c) => c
    }.toSet
    val sumOfSomewhere: Set[Char] = (this.somewhere ++ other.somewhere) -- knownLetters

    GuessResult(
      sumOfPositionals,
      this.knownAbsent ++ other.knownAbsent,
      sumOfSomewhere
    )
  }
}

object KnowNothing extends GuessResult(
  Array.fill(5)(Unknown).toList,
  Set(),
  Set()
)

object GuessResult {

  /**
   * Response format
   * <pre>
   * v: correct
   * x: nowhere
   * ~: somewhere else
   * </pre>
   */
  def parse(play: String, response: String): GuessResult = {
    (play zip response).zipWithIndex.collect {
      case ((c, 'v'), i) => knowOneLetterAt(c, i)
      case ((c, 'x'), _) => no(c)
      case ((c, '~'), i) => elsewhere(c, i)
    }.reduce(_ + _)
  }

  private def knowOneLetterAt(c: Char, i: Int): GuessResult =
    GuessResult(
      Array.fill(5)(Unknown).toList.updated(i, Known(c)),
      Set(),
      Set()
    )

  private def no(c: Char): GuessResult =
    GuessResult(
      Array.fill(5)(Unknown).toList,
      Set(c),
      Set()
    )

  def elsewhere(c: Char, i: Int): GuessResult =
    GuessResult(
      Array.fill(5)(Unknown).toList.updated(i, Not(Set(c))),
      Set(),
      Set(c)
    )
}
