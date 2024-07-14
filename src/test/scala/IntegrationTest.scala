package com.ericriese.scwordle

import org.scalatest.funsuite.AnyFunSuite

class IntegrationTest extends AnyFunSuite {

  test("removes share after we learn it doesn't start with s") {
    val guesses = Guesses(Seq("stand", "share"))
    guesses.filterInPlace(new Filterer(Clue.parse("stand", "x~v~x")))
    assert(guesses.isEmpty)
  }

  test("rules out share after we learn it doesn't start with s") {
    val f = new Filterer(Clue.parse("stand", "x~v~x"))
    assert(!f("share"))
  }

  test("parses Clue") {
    val clue = Clue.parse("stand", "x~v~x")
    println(clue)
    assert(clue == Clue(
      List(
        Not(Set('s', 'd')),
        Not(Set('t', 's', 'd')),
        Known('a'),
        Not(Set('s', 'n', 'd')),
        Not(Set('s', 'd'))
      ),
      Set('t', 'n')
    ))
  }

  test("clears out guesses after all filterer") {
    val guesses = Guesses(Seq("stand", "share"))
    guesses.filterInPlace(s => false)
    assert(guesses.isEmpty)
  }

  test("empty guesses is empty") {
    assert(Guesses(Seq()).isEmpty)
  }
}
