/*
Copyright 2017-2018 EconomicSL

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package org.economicsl.mechanisms.voting

import org.economicsl.mechanisms.Alternative


case class VoteCounts[A <: Alternative](counts: Map[A, Int]) {

  def updated[B <: Ballot[A]](ballot: B): VoteCounts[A] = {
    val votes = counts.getOrElse(ballot.alternative, 0)
    VoteCounts(counts.updated(ballot.alternative, votes + 1))
  }

  def combine(other: VoteCounts[A]): VoteCounts[A] = {
    val results = counts.foldLeft(other.counts){ case (totals, (alternative, votes)) =>
      val additionalVotes = other.counts.getOrElse(alternative, 0)
      totals.updated(alternative, votes + additionalVotes)
    }
    VoteCounts(results)
  }

  def mostPreferred: A = {
    val (alternative, _) = counts.maxBy{ case (_, votes) => votes }
    alternative
  }

}


object VoteCounts {

  def empty[A <: Alternative]: VoteCounts[A] = {
    VoteCounts(Map.empty[A, Int])
  }

}
