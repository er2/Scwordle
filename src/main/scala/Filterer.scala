package com.ericriese.scwordle

import scala.util.matching.Regex

class Filterer(guessResult: GuessResult) extends (String => Boolean) {

  val positionalRegex: Regex = positionalsToRegex
  val absentRegex: Regex = (makeAbsentRegex(guessResult.notPresent) + "+").r
  val somewhereRegexes: Set[Regex] = makeSomewhereRegexes

  override def apply(w: String): Boolean = {
    positionalRegex.matches(w) && absentRegex.matches(w) && somewhereRegexes.forall(_.matches(w))
  }

  private def positionalsToRegex: Regex = {
    val regex = guessResult.positional.map({
      case Known(c) => c
      case Not(s) => makeAbsentRegex(s)
      case Unknown => "."
    }).mkString
    regex.r
  }

  private def makeSomewhereRegexes = guessResult.somewheres.map(ch => (".*(" + ch + ").*").r)

  private def makeAbsentRegex(s: Set[Char]) = if (s.isEmpty) "." else "[^" + s.mkString + "]"
}
