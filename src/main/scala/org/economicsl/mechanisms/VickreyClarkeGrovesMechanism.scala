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

import scala.collection.{GenMap, GenSet}


/** Direct mechanism is a social choice function and a collection of payment functions used to provide incentives to players. */
trait DirectMechanism[A <: Alternative] extends SocialChoiceFunction[A, ValuationFunction[A]] {

  def payments: GenSet[PaymentFunction[A]]

}


/** Class representing a Vickrey-Clarke-Groves (VCG) mechanism. */
class VickreyClarkeGrovesMechanism[A <: Alternative](hs: GenSet[PaymentFunction[A]]) {

  def apply(alternatives: GenSet[A], valuations: GenSet[ValuationFunction[A]]): A = {
    val socialValuation = VickreyClarkeGrovesMechanism.aggregate(valuations)
    alternatives.reduce((a, b) => if (socialValuation(a) < socialValuation(b)) b else a)
  }

  def payments(alternative: A, valuations: GenSet[ValuationFunction[A]]): GenMap[UUID, Money] = {
    val socialValuation = VickreyClarkeGrovesMechanism.aggregate(valuations)
    hs.map( h =>
      val valuation = valuations(uuid)
      val otherValuations = valuations - uuid
      val payment = h(otherValuations) - (socialValuation(alternative) - valuation(alternative))
      (uuid, payment)
    }
  }

}


object VickreyClarkeGrovesMechanism {

  def apply[A <: Alternative](hs: GenMap[UUID, PaymentFunction[A]]): VickreyClarkeGrovesMechanism[A] = {
    new VickreyClarkeGrovesMechanism(hs)
  }

  def clarkePivotRule[A <: Alternative](alternatives: GenSet[A]): PaymentFunction[A] = {
    new PaymentFunction[A] {
      def apply(valuations: GenSet[ValuationFunction[A]]): Money = {
        val socialValuation = aggregate(valuations)
        alternatives.aggregate(Double.MinValue)((maxValue, a) => maxValue max socialValuation(a), _ max _)
      }
    }
  }

  def aggregate[A <: Alternative](valuations: GenSet[ValuationFunction[A]]): ValuationFunction[A] = {
    new ValuationFunction[A] {
      def apply(alternative: A): Money = {
        valuations.aggregate(0.0)((socialValue, v) => socialValue + v(alternative), _ + _)
      }
    }
  }

}

