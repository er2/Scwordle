package com.ericriese.scwordle

class Plays(previousPlays: List[String], guesses: Guesses) extends PlaySource:

  override def next(filterer: String => Boolean): (String, PlaySource) =
    val newGuesses = guesses.filter(filterer)
    (previousPlays.head, Plays(previousPlays.tail, newGuesses))

  override def isEmpty: Boolean = previousPlays.isEmpty && guesses.isEmpty

object Plays:
  def apply(previousPlays: List[String], guesses: Guesses): PlaySource =
    previousPlays match
      case Nil => guesses
      case _ => new Plays(previousPlays, guesses)