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


trait PluralityVotingRule
  extends SocialChoiceFunction[Iterable[Ballot], Iterable[Alternative], Alternative] {

  def apply(preferences: Iterable[Ballot]): Alternative = {
    val results = preferences.aggregate(VoteCounts.empty)(
      (counts, ballot) => counts.updated(ballot),
      (m1, m2) => m1.combine(m2)
    )
    results.mostPreferred
  }

}
