package com.ericriese.scwordle

import scala.util.matching.Regex

class Filterer(clue: Clue) extends (String => Boolean):

  override def apply(w: String): Boolean =
    (positionalRegex :: somewhereRegexes).forall(_.matches(w))

  val positionalRegex: Regex =
    clue.positional.map {
      case Known(c) => c
      case Not(s) => absentRegex(s)
      case Unknown => "."
    }.mkString.r

  val somewhereRegexes: List[Regex] =
    clue.somewheres.toList.map(ch => (".*(" + ch + ").*").r)

  private def absentRegex(s: Set[Char]): String =
    if (s.isEmpty) "."
    else "[^" + s.mkString + "]"
