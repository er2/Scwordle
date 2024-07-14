package com.ericriese.scwordle

import org.scalatest.funsuite.AnyFunSuite

class IntegrationTest extends AnyFunSuite:

  test("removes share after we learn it doesn't start with s"):
    val guesses = Guesses(Seq("stand", "share"))
    val newGuesses = guesses.filter(new Filterer(Clue.parse("stand", "x~v~x")))
    assert(newGuesses.isEmpty)

  test("removes soare after we learn contain r o s e"):
    val guesses = Guesses(Seq("soare"))
    val newGuesses = guesses.filter(new Filterer(Clue.parse("arose", "~xxxx")))
    assert(newGuesses.isEmpty)

  test("rules out share after we learn it doesn't start with s"):
    val f = new Filterer(Clue.parse("stand", "x~v~x"))
    assert(!f("share"))

  test("parses Clue"):
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

  test("clears out guesses after all filterer"):
    val guesses = Guesses(Seq("stand", "share"))
    val newGuesses = guesses.filter(s => false)
    assert(newGuesses.isEmpty)

  test("empty guesses is empty"):
    val guesses = Guesses(Seq())
    assert(guesses.isEmpty)