package org.economicsl.mechanisms.voting

import java.util.UUID
import org.economicsl.mechanisms.{Alternative, Preference}


case class Ballot(signature: UUID, alternative: Alternative)
  extends Preference[Alternative] {

  /** Returns an integer whose sign communicates how `a1` compares to `a2`.
    *
    * The result sign has the following meaning:
    * - negative if `a2` is preferred to `a1`.
    * - positive if `a1` is weakly preferred to `a2`.
    * - zero if indifferent between `a1` and `a2`
    */
  def compare[A <: Alternative](a1: A, a2: A): Int = {
    if (a2 == alternative) {
      -1
    } else if (a1 == alternative) {
      1
    } else {
      0
    }
  }

}
