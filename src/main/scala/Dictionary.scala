package com.ericriese.scwordle

import scala.collection.mutable
import scala.io.Source

object Dictionary {

  /** read, cleanup and filter out all but 5 letter words */
  def apply(source: Source): Seq[String] = {
    val dictionary = source.getLines()
    val fives = dictionary
      .filter(!_.contains("'"))
      .filter(_.length == 5)
      .filter(_(0).isLower)
    val canonicalized = removeAdjacentDuplicates(fives)
    canonicalized
  }

  private def removeAdjacentDuplicates(in: Iterator[String]): Seq[String] = {
    in.foldLeft(List[String]())((list, word) => {
      if (list.isEmpty)
        word :: list
      else if (word.equalsIgnoreCase(list.head))
        list
      else
        word :: list
    })
  }
}
