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
package org.economicsl.mechanisms

<<<<<<< HEAD
import scala.collection.{GenIterable, GenSeq, GenSet}

=======
>>>>>>> master

/** Base trait defining a generic social choice function.
  *
  * A social choice function aggregates a collection of preferences and returns
  * a single alternative.
  */
<<<<<<< HEAD
trait SocialChoiceFunction[-CC <: GenIterable[_ >: Preference[A]], A <: Alternative]
  extends (CC => A) {
=======
trait SocialChoiceFunction[-CC1 <: Iterable[P], +P <: Preference[A], -CC2 <: Iterable[A], A] {

  def apply(preferences: CC1)(alternatives: CC2): A
>>>>>>> master

  /** See definition 9.9 in Algorithmic Game Theory. */
  def extend(alternatives: CC2): SocialWelfareFunction[CC1, P, A]

  def extend: SocialWelfareFunction[CC, _ >: Preference[A]]

}


object SocialChoiceFunction {

  def dictator[A <: Alternative, P <: Preference[A]]
              (i: Int)(alternatives: GenSet[A])
              : SocialChoiceFunction[GenSeq[P], A] = {
    new SocialChoiceFunction[GenSeq[P], A] {
      def apply(preferences: GenSeq[P]): A = {
        alternatives.max(preferences(i).ordering)
      }
      def extend: SocialWelfareFunction[GenSeq[P], P] = {
        new SocialWelfareFunction[GenSeq[P], P] {
          def apply(preferences: GenSeq[P]): P = {
            preferences(i)
          }
        }
      }
    }
  }

}
