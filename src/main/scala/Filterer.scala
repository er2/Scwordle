package com.ericriese.scwordle

import scala.util.matching.Regex

class Filterer(guessResult: GuessResult) extends (String => Boolean) {

  val positionalRegex: Regex = makePositionalRegex
  val absentRegex: Regex = makeAbsentRegex
  val somewhereRegexes: List[Regex] = makeSomewhereRegexes

  override def apply(w: String): Boolean = {
    (positionalRegex :: absentRegex :: somewhereRegexes).forall(_.matches(w))
  }

  private def makePositionalRegex: Regex = {
    val regex = guessResult.positional.map({
      case Known(c) => c
      case Not(s) => absentRegex(s)
      case Unknown => "."
    }).mkString
    regex.r
  }

  private def makeAbsentRegex = (absentRegex(guessResult.notPresent) + "+").r

  private def makeSomewhereRegexes = guessResult.somewheres.toList.map(ch => (".*(" + ch + ").*").r)

  private def absentRegex(s: Set[Char]): String = if (s.isEmpty) "." else "[^" + s.mkString + "]"
}
