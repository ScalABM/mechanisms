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

import cats._
import implicits._


/** Base trait defining a generic cardinal welfare function.
  *
  * A cardinal social welfare function is a function that takes as input
  * numeric representations of individual utilities (also known as cardinal
  * utility), and returns as output a numeric representation of the collective
  * welfare. The underlying assumption is that individuals utilities can be put
  * on a common scale and compared.
  */
trait CardinalSocialWelfareFunction[-CC <: Iterable[ValuationFunction[A]], A]
  extends SocialWelfareFunction[CC, ValuationFunction[A], A]


object CardinalSocialWelfareFunction {

  def average[CC <: Iterable[ValuationFunction[A]], A]
             : CardinalSocialWelfareFunction[CC, A] = {
    new CardinalSocialWelfareFunction[CC, A] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            val (total, count) = preferences.map(v => (v(a), 1)).reduce(_ |+| _)
            total / count
          }
        }
      }
    }
  }

  /** Rawlsian social welfare function: society should maximize the minimum individual Numeraire. */
  def min[CC <: Iterable[ValuationFunction[A]], A]
         : CardinalSocialWelfareFunction[CC, A] = {
    new CardinalSocialWelfareFunction[CC, A] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.reduce(ValuationFunction.min[A].combine)(a)
          }
        }
      }
    }
  }

  /** Nash bargaining maximizes the produce of individual utilities. */
  def product[CC <: Iterable[ValuationFunction[A]], A]
             : CardinalSocialWelfareFunction[CC, A] = {
    new CardinalSocialWelfareFunction[CC, A] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.reduce(ValuationFunction.prod[A].combine)(a)
          }
        }
      }
    }
  }

  /** Benthamite social welfare function: society should maximize the sum of individual Numeraire. */
  def sum[CC <: Iterable[ValuationFunction[A]], A]
         : CardinalSocialWelfareFunction[CC, A]  = {
    new CardinalSocialWelfareFunction[CC, A] {
      def apply(preferences: CC): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            preferences.reduce(ValuationFunction.sum[A].combine)(a)
          }
        }
      }
    }
  }

}
