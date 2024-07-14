package com.ericriese.scwordle

import scala.collection.mutable
import scala.util.Random

class UniqueRandoms(
                     random: Random = new Random(),
                     seen: mutable.Set[Double] = mutable.Set[Double]()
                   ):
  def next(): Double =
    val d = random.nextDouble()
    if (!seen.add(d))
      next()
    d