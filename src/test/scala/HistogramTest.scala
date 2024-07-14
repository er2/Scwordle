package com.ericriese.scwordle

import org.scalatest.funsuite.AnyFunSuite

class HistogramTest extends AnyFunSuite:

  test("sorts by commonality of letters"):
    val testDict = Seq("aaaaa", "bbbcc", "vwxyz").reverse
    val histogram = new Histogram(testDict)
    val sorted = histogram.sort(testDict)
    assert(sorted.toSeq == Seq("aaaaa", "bbbcc", "vwxyz"))
    assert(sorted.head == "aaaaa")