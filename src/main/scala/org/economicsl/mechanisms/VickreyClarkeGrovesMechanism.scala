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

import scala.collection.{GenIterable, GenMap, GenSet}


/** Class representing a Vickrey-Clarke-Groves (VCG) mechanism. */
class VickreyClarkeGrovesMechanism[A <: Alternative](
  alternatives: GenSet[A],
  hs: GenMap[UUID, PaymentFunction[GenIterable[ValuationFunction[A]]]])
    extends SocialChoiceFunction[GenIterable[ValuationFunction[A]], A]{

  def apply(preferences: GenIterable[ValuationFunction[A]]): A = {
    val socialValuation = VickreyClarkeGrovesMechanism.aggregate(preferences)
    alternatives.reduce((a, b) => if (socialValuation(a) < socialValuation(b)) b else a)
  }

  def payments(alternative: A, valuations: GenMap[UUID, ValuationFunction[A]]): GenMap[UUID, Numeraire] = {
    val socialValuation = VickreyClarkeGrovesMechanism.aggregate(valuations.values)
    hs.map { case (uuid, h) =>
      val valuation = valuations(uuid)
      val otherValuations = valuations - uuid
      val payment = h(otherValuations.values) - (socialValuation(alternative) - valuation(alternative))
      (uuid, payment)
    }
  }

}


object VickreyClarkeGrovesMechanism {

  def apply[A <: Alternative]
           (alternatives: GenSet[A], hs: GenMap[UUID, PaymentFunction[GenIterable[ValuationFunction[A]]]])
           : VickreyClarkeGrovesMechanism[A] = {
    new VickreyClarkeGrovesMechanism(alternatives, hs)
  }

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

  def aggregate[A <: Alternative]
               (preferences: GenIterable[ValuationFunction[A]])
               : ValuationFunction[A] = {
    new ValuationFunction[A] {
      def apply(alternative: A): Numeraire = {
        preferences.aggregate(0L)((socialValue, v) => socialValue + v(alternative), _ + _)
      }
    }
  }

}
