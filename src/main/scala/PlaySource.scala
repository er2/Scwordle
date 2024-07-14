package com.ericriese.scwordle

trait PlaySource:
  def next(): (String, PlaySource) = next(_ => true)
  def next(filterer: String => Boolean): (String, PlaySource)
  def isEmpty: Boolean
