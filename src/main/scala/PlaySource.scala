package com.ericriese.scwordle

trait PlaySource:
  def next(): (String, PlaySource)
  def next(filterer: String => Boolean): (String, PlaySource)
  def isEmpty: Boolean
