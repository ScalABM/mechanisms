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

import cats.{Monoid, Semigroup}


/** Base trait defining a generic social welfare function.
  *
  * A social welfare function aggregates the preferences of individual agents
  * into a common preference ordering.
  */
trait SocialWelfareFunction[-CC <: Iterable[P], +P <: Preference[A], A]
  extends (CC => P)


object SocialWelfareFunction {

  def lexicographic[A]: SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] = {
    new SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] {
      def apply(preferences: Iterable[Preference[A]]): Preference[A] = {
        preferences.reduce(Preference.leftBiasedWhenIndifferent[A].combine)
      }
    }
  }

  /** Define a `SocialWelfareFunction` using an available `Monoid[Preference[A]]`. */
  def fold[A](implicit ev: Monoid[Preference[A]]): SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] = {
    new SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] {
      def apply(preferences: Iterable[Preference[A]]): Preference[A] = {
        preferences.fold(ev.empty)(ev.combine)
      }
    }
  }

  /** Define a `SocialWelfareFunction` using an available `Semigroup[Preference[A]]`. */
  def reduce[A](implicit ev: Semigroup[Preference[A]]): SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] = {
    new SocialWelfareFunction[Iterable[Preference[A]], Preference[A], A] {
      def apply(preferences: Iterable[Preference[A]]): Preference[A] = {
        preferences.reduce(ev.combine)
      }
    }
  }
}
