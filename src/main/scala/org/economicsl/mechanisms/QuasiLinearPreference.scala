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
import cats.implicits._


/** Class representing a quasi-linear preferences.
  *
  * @param v a `ValuationFunction` mapping an alternative of type `A` to a value
  *          in units of some `Numeraire`.
  * @tparam A the type of `Alternative` over which the function `v` as defined.
  */
case class QuasiLinearPreference[-A](v: ValuationFunction[A])
  extends Preference[A] with Function2[A, Numeraire, Numeraire] {

  def apply(alternative: A, payment: Numeraire): Numeraire = {
    payment + v(alternative)
  }

  def compare(a1: A, a2: A): Int = {
    ordering.compare(a1, a2)
  }

}


object QuasiLinearPreference {

  implicit val contravariant: Contravariant[QuasiLinearPreference] = {
    new Contravariant[QuasiLinearPreference] {
      def contramap[A, B](fa: QuasiLinearPreference[A])(f: B => A): QuasiLinearPreference[B] = {
        val valuationFunction = new ValuationFunction[B] {
          def apply(alternative: B): Numeraire = {
             fa.v(f(alternative))
          }
        }
        new QuasiLinearPreference[B](valuationFunction)
      }
    }
  }

}
