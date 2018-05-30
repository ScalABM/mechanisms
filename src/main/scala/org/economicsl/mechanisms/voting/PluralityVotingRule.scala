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

import org.economicsl.mechanisms.{Alternative, SocialChoiceFunction}
import scala.collection.GenSet


class PluralityVotingRule(initial: VoteCounts)
  extends SocialChoiceFunction[Alternative, Ballot] {

  def apply[A <: Alternative, P <: Ballot](preferences: GenSet[P]): A = {
    val results = preferences.aggregate(initial)((counts, ballot) => update(counts, ballot), (m1, m2) => combine(m1, m2))
    val (winner, _) = results maxBy { case (_, votes) => votes }  // todo explicitly handle ties!
    winner
  }

  private[this] def update(current: VoteCounts, ballot: Ballot): VoteCounts = {
    val votes = current.getOrElse(ballot.alternative, 0)
    current.updated(ballot.alternative, votes + 1)
  }

  private[this] def combine(some: VoteCounts, other: VoteCounts): VoteCounts = {
    some.foldLeft(other){ case (counts, (candidate, votes)) =>
      val additionalVotes = other.getOrElse(candidate, 0)
      counts.updated(candidate, votes + additionalVotes)
    }
  }

}
