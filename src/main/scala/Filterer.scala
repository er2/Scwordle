package com.ericriese.scwordle

import scala.util.matching.Regex

class Filterer(clue: Clue) extends (String => Boolean) {

  val positionalRegex: Regex = makePositionalRegex
  val somewhereRegexes: List[Regex] = makeSomewhereRegexes

  override def apply(w: String): Boolean = {
    (positionalRegex :: somewhereRegexes).forall(_.matches(w))
  }

  private def makePositionalRegex: Regex = {
    clue.positional.map {
      case Known(c) => c
      case Not(s) => absentRegex(s)
      case Unknown => "."
    }.mkString.r
  }

  private def makeSomewhereRegexes = {
    clue.somewheres.toList.map(ch => (".*(" + ch + ").*").r)
  }

  private def absentRegex(s: Set[Char]): String = {
    if (s.isEmpty) "."
    else "[^" + s.mkString + "]"
  }
}
