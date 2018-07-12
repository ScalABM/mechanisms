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

import cats._
import cats.implicits._

import org.economicsl.mechanisms.{Preference, SocialChoiceFunction, SocialWelfareFunction}


trait VotingRule[A] extends SocialChoiceFunction[Set[Preference[A]], Preference[A], Set[A], A] {
  self =>

  final def extend(alternatives: Set[A]): SocialWelfareFunction[Set[Preference[A]], Preference[A], A] = {
    new SocialWelfareFunction[Set[Preference[A]], Preference[A], A] {
      def apply(preferences: Set[Preference[A]]): Preference[A] = {
        new Preference[A] {
          def compare(a1: A, a2: A): Int = {
            if (!alternatives.exists(_ == a1) & alternatives.exists(_ == a2)) {
              -1
            } else if (alternatives.exists(_ == a1) & !alternatives.exists(_ == a2)) {
              1
            } else {
              if (self.apply(preferences)(Set(a1, a2)) == a2) -1 else 1
            }
          }
        }
      }
    }
  }

}


object VotingRule {

  def majority[A]: VotingRule[Option[A]] = {
    new VotingRule[Option[A]] {
      def apply(preferences: Set[Preference[Option[A]]])(alternatives: Set[Option[A]]): Option[A] = {
        val mostPreferredAlternatives = preferences.map(p => Map(p.mostPreferred(alternatives) -> 1))
        val voteCounts = mostPreferredAlternatives.reduce(_ |+| _)
        val majorityPreferredAlternative = {
          voteCounts.find{
            case (_, count) => count > (preferences.size / 2)
          }.flatMap{
            case (alternative, _) => alternative
          }
        }
        majorityPreferredAlternative
      }
    }
  }

  def plurality[A]: VotingRule[A] = {
    new VotingRule[A] {
      def apply(preferences: Set[Preference[A]])(alternatives: Set[A]): A = {
        val mostPreferredAlternatives = preferences.map(p => Map(p.mostPreferred(alternatives) -> 1))
        val voteCounts = mostPreferredAlternatives.reduce(_ |+| _)
        val (mostPreferredAlternative, _) = voteCounts.maxBy{ case (_, count) => count }
        mostPreferredAlternative
      }
    }
  }

  def bordaCount[A]: VotingRule[A] = {
    new VotingRule[A] {
      def apply(preferences: Set[Preference[A]])(alternatives: Set[A]): A = {
        val rankings = preferences.map(p => p.rank(alternatives))
        val voteCounts = rankings.reduce(_ |+| _)
        val (mostPreferredAlternative, _) = voteCounts.maxBy{ case (_, count) => count }
        mostPreferredAlternative
      }
    }
  }

}
