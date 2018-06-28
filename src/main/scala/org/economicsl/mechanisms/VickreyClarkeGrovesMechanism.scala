/*
Copyright 2017 EconomicSL

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

import java.util.UUID

import scala.collection.{GenIterable, GenMap, GenSeq, GenSet}
import scala.collection.immutable.{HashSet, Vector}
import scala.collection.parallel.immutable.{ParHashSet, ParVector}


/** trait representing a Vickrey-Clarke-Groves (VCG) mechanism. */
trait VickreyClarkeGrovesMechanism[-CC1 <: GenSeq[ValuationFunction[A]],
                                   -CC2 <: GenSeq[PaymentFunction[CC1]],
                                   A <: Alternative,
                                   CC3 <: GenSeq[Numeraire]]
  extends DirectRevelationMechanism[CC1, CC2, A, CC3] {

    def apply(preferences: CC1): A = {
      val socialWelfareFunction = extend(preferences)
      alternatives.reduce((a, b) => if (socialWelfareFunction(a) < socialWelfareFunction(b)) b else a)
    }

    def apply(preferences: CC1, paymentFunctions: CC2): (A, CC3) = {
      val mostPreferredAlternative = apply(preferences)
      val payments = paymentFunctions.zipWithIndex.map { case (h, i) =>
        val playerValuation(i) = preferences(i)
        val otherPlayerValuations = preferences.patch(i, Nil, 1)
        val otherPlayersSocialValuation = extend(otherPlayerValuations)
        h(otherPlayerValuations) - otherPlayersSocialValuation(mostPreferredAlternative)
      }
      (mostPreferredAlternative, payments)
    }

    def extend: SocialWelfareFunction[CC1, ValuationFunction[A]] = {
      new SocialWelfareFunction[CC1, ValuationFunction[A]] {
        def apply(preferences: CC1): ValuationFunction[A] = {
          new ValuationFunction[A] {
            def apply(alternative: A): Numeraire = {
              preferences.map(p => p(alternative)).reduce(_ + _)
            }
          }
        }
      }
    }

  }


object VickreyClarkeGrovesMechanism {

  type ValuationFunctions[A <: Alternative] = Vector[A]
  type PaymentFunctions[A <: Alternative] = Vector[ValuationFunctions[A]]
  type Payments = Vector[Numeraire]

  type ParValuationFunctions[A <: Alternative] = ParVector[A]
  type ParPaymentFunctions[A <: Alternative] = ParVector[ParValuationFunctions[A]]
  type ParPayments = ParVector[Numeraire]

  def apply[A <: Alternative](alternatives: HashSet[A])
           : VickreyClarkeGrovesMechanism[ValuationFunctions[A], PaymentFunctions[A], A, Payments] = {
    VickreyClarkeGrovesMechanismImpl(alternatives)
  }

  def apply[A <: Alternative](alternatives: ParHashSet[A])
           : VickreyClarkeGrovesMechanism[ParValuationFunctions[A], ParPaymentFunctions[A], A, ParPayments] = {
    ParVickreyClarkeGrovesMechanismImpl(alternatives)
  }

  private[this] case class VickreyClarkeGrovesMechanismImpl[A <: Alternative](alternatives: HashSet[A])
    extends VickreyClarkeGrovesMechanism[ValuationFunctions[A], PaymentFunctions[A], A, Payments]

  private[this] case class ParVickreyClarkeGrovesMechanismImpl[A <: Alternative](alternatives: ParHashSet[A])
    extends VickreyClarkeGrovesMechanism[ParValuationFunctions[A], ParPaymentFunctions[A], A, ParPayments]

  def clarkePivotRule[A <: Alternative]
                     (alternatives: GenSet[A])
                     : PaymentFunction[GenIterable[ValuationFunction[A]]] = {
    new PaymentFunction[GenIterable[ValuationFunction[A]]] {
      def apply(valuations: GenIterable[ValuationFunction[A]]): Numeraire = {
        val socialValuation = aggregate(valuations)
        alternatives.aggregate(Long.MinValue)((maxValue, a) => maxValue max socialValuation(a), _ max _)
      }
    }
  }

}
