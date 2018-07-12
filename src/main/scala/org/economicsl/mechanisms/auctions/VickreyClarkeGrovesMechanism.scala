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
package org.economicsl.mechanisms.auctions

import org.economicsl.mechanisms.ValuationFunction


/** trait representing a Vickrey-Clarke-Groves (VCG) mechanism. */
trait VickreyClarkeGrovesMechanism[A] extends DirectRevelationMechanism[A] {

    def apply(preferences: ValuationFunctions[A])(alternatives: Vector[A])(paymentFunctions: PaymentFunctions[A]): (A, Payments) = {
      val socialWelfareFunction = preferences.reduce(ValuationFunction.sum[A].combine)
      val socialValuations = alternatives.map(a => (a, socialWelfareFunction(a)))
      val (mostPreferredAlternative, socialValuation) = socialValuations.maxBy{ case (_, valuation) => valuation }
      val payments = paymentFunctions.zipWithIndex.map { case (h, i) =>
        val valuation = preferences(i)
        val otherValuations = preferences.patch(i, Nil, 1)
        h(otherValuations) - (socialValuation - valuation(mostPreferredAlternative))
      }
      (mostPreferredAlternative, payments)
    }

    def withClarkePivotRule(preferences: ValuationFunctions[A])(alternatives: Vector[A]): (A, Payments) = {
      val paymentFunctions = preferences.map(p => clarkePivotRule(alternatives))
      apply(preferences)(alternatives)(paymentFunctions)
    }

    private[this] def clarkePivotRule(alternatives: Vector[A]) = {
      new PaymentFunction[A] {
        def apply(preferences: Vector[ValuationFunction[A]]): Numeraire = {
          val socialWelfareFunction = preferences.reduce(ValuationFunction.sum[A].combine)
          val socialValuations = alternatives.map(a => socialWelfareFunction(a))
          socialValuations.max
        }
      }
    }
  }
