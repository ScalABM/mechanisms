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


/** trait representing a Vickrey-Clarke-Groves (VCG) mechanism. */
trait VickreyClarkeGrovesMechanism[A <: Alternative]
  extends DirectRevelationMechanism[A] {

    def apply(preferences: ValuationFunctions[A]): A = {
      val socialWelfareFunction = extend(preferences)
      alternatives.reduce((a, b) => if (socialWelfareFunction(a) < socialWelfareFunction(b)) b else a)
    }

    def apply(preferences: ValuationFunctions[A], paymentFunctions: PaymentFunctions[A]): (A, Payments) = {
      val mostPreferredAlternative = apply(preferences)
      val payments = paymentFunctions.zipWithIndex.map { case (h, i) =>
        val otherPlayerValuations = preferences.patch(i, Nil, 1)
        val otherPlayersSocialValuation = extend(otherPlayerValuations)
        h(otherPlayerValuations)- otherPlayersSocialValuation(mostPreferredAlternative)
      }
      (mostPreferredAlternative, payments)
    }

    def extend: SocialWelfareFunction[ValuationFunctions[A], ValuationFunction[A]] = {
      new SocialWelfareFunction[ValuationFunctions[A], ValuationFunction[A]] {
        def apply(preferences: ValuationFunctions[A]): ValuationFunction[A] = {
          new ValuationFunction[A] {
            def apply(alternative: A): Numeraire = {
              preferences.map(p => p(alternative)).reduce(_ + _)
            }
          }
        }
      }
    }

  }
