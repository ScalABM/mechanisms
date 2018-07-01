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

import scala.collection.GenIterable


/** Base trait defining a generic cardinal welfare function.
  *
  * A cardinal social welfare function is a function that takes as input
  * numeric representations of individual utilities (also known as cardinal
  * utility), and returns as output a numeric representation of the collective
  * welfare. The underlying assumption is that individuals utilities can be put
  * on a common scale and compared.
  */
trait CardinalSocialWelfareFunction[-CC <: GenIterable[P], +P <: ValuationFunction[_ <: Alternative]]
  extends SocialWelfareFunction[CC, P]


object CardinalSocialWelfareFunction {

  def average[CC <: GenIterable[P], P <: ValuationFunction[A], A <: Alternative]
             : CardinalSocialWelfareFunction[CC, ValuationFunction[A]] = {
    new CardinalSocialWelfareFunction[CC, ValuationFunction[A]] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            val (total, count) = {
              preferences.map(v => (v(a), 1))
                .reduce[(Numeraire, Int)]{
                  case ((n0, d0), (n1, d1)) => (n0 + n1, d0 + d1) // simplifed using cats!
                }
              }
            total / count
          }
        }
      }
    }
  }

  /** Rawlsian social welfare function: society should maximize the minimum individual Numeraire. */
  def min[CC <: GenIterable[P], P <: ValuationFunction[A], A <: Alternative]
         : CardinalSocialWelfareFunction[CC, ValuationFunction[A]] = {
    new CardinalSocialWelfareFunction[CC, ValuationFunction[A]] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.map(v => v(a)).reduce(_ min _)
          }
        }
      }
    }
  }

  /** Nash bargaining maximizes the produce of individual utitlities. */
  def product[CC <: GenIterable[P], P <: ValuationFunction[A], A <: Alternative]
             : CardinalSocialWelfareFunction[CC, ValuationFunction[A]] = {
    new CardinalSocialWelfareFunction[CC, ValuationFunction[A]] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.map(v => v(a)).reduce(_ * _)
          }
        }
      }
    }
  }

  /** Benthamite social welfare function: society should maximize the sum of individual Numeraire. */
  def sum[CC <: GenIterable[P], P <: ValuationFunction[A], A <: Alternative]
         : CardinalSocialWelfareFunction[CC, ValuationFunction[A]]  = {
    new CardinalSocialWelfareFunction[CC, ValuationFunction[A]] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.map(v => v(a)).reduce(_ + _)
          }
        }
      }
    }
  }

}
