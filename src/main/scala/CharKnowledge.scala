package com.ericriese.scwordle

sealed trait CharKnowledge

case class Known(c : Char) extends CharKnowledge

case class Not(c : Set[Char]) extends CharKnowledge

case object Unknown extends CharKnowledge