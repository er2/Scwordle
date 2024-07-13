package com.ericriese.scwordle

sealed trait CharKnowledge

case class Known(c : Char) extends CharKnowledge

case class Not(c : Set[Char]) extends CharKnowledge

object Not:
  def apply(c: Char): Not = Not(Set(c))

case object Unknown extends CharKnowledge