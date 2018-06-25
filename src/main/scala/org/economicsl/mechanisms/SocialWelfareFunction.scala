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

import scala.collection.{GenIterable, GenSet}


/** Base trait defining a generic social welfare function.
  *
  * A social welfare function aggregates the preferences of individual agents
  * into a common preference ordering.
  */
trait SocialWelfareFunction[-CC <: GenIterable[P], +P <: Preference[_ <: Alternative]]
  extends (CC => P) {

  def apply(preferences: CC): P

}


object SocialWelfareFunction {

  def average[A <: Alternative]: SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] = {
    new SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] {
      def apply(preferences: GenSet[UtilityFunction[A]]): UtilityFunction[A] = {
        new UtilityFunction[A] {
          def apply(a: A): Utility = {
            val (n, d) = preferences.aggregate((0L, 0))(
              { case ((total, count), u) => (total + u(a), count + 1) },
              { case ((n0, d0), (n1, d1)) => (n0 + n1, d0 + d1) }
            )
            n / d
          }
        }
      }
    }
  }

  /** Rawlsian social welfare function: society should maximize the minimum individual utility. */
  def min[A <: Alternative]: SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] = {
    new SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] {
      def apply(preferences: GenSet[UtilityFunction[A]]): UtilityFunction[A] = {
        new UtilityFunction[A] {
          def apply(a: A): Utility = {
            preferences.aggregate(0L)((min, u) => if (u(a) < min) u(a) else min, _ min _)
          }
        }
      }
    }
  }

  /** Nash bargaining maximizes the produce of individual utitlities. */
  def product[A <: Alternative]: SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] = {
    new SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] {
      def apply(preferences: GenSet[UtilityFunction[A]]): UtilityFunction[A] = {
        new UtilityFunction[A] {
          def apply(a: A): Utility = {
            preferences.aggregate(1L)((acc, u) => acc * u(a), _ * _)
          }
        }
      }
    }
  }

  /** Benthamite social welfare function: society should maximize the sum of individual utility. */
  def sum[A <: Alternative]: SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] = {
    new SocialWelfareFunction[GenSet[UtilityFunction[A]], UtilityFunction[A]] {
      def apply(preferences: GenSet[UtilityFunction[A]]): UtilityFunction[A] = {
        new UtilityFunction[A] {
          def apply(a: A): Utility = {
            preferences.aggregate(0L)((total, u) => total + u(a), _ + _)
          }
        }
      }
    }
  }

}
