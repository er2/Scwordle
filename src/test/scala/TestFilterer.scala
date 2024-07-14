package com.ericriese.scwordle

import org.scalatest.funsuite.AnyFunSuite

class TestFilterer extends AnyFunSuite:

  val startsWithR: Clue = Clue(
    positional = List(Known('r'), Unknown, Unknown, Unknown, Unknown),
    somewheres = Set()
  )

  test("filterer creates correct regex"):
    assert(new Filterer(startsWithR).positionalRegex.toString() == "r....")

  val doesntStartWithA: Clue = Clue(
    positional = List(Not(Set('a')), Unknown, Unknown, Unknown, Unknown),
    somewheres = Set()
  )

  test("filterer creates correct regex for negation"):
    assert(new Filterer(doesntStartWithA).positionalRegex.toString() == "[^a]....")

  test("guessresults combine"):
    assert(new Filterer(startsWithR + doesntStartWithA).positionalRegex.toString() == "r....")
