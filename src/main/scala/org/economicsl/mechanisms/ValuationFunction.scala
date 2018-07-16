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


/** Base trait for representing preferences defined over alternatives in terms
  * of each alternative's monetary value.
  */
trait ValuationFunction[A]
  extends Preference[A] with ((A) => Numeraire) {

  def compare(a1: A, a2: A): Int = {
    apply(a1).compare(apply(a2))
  }

}


object ValuationFunction {

  implicit val contravariant: Contravariant[ValuationFunction] = {
    new Contravariant[ValuationFunction] {
      def contramap[A, B](fa: ValuationFunction[A])(f: B => A): ValuationFunction[B] = {
        new ValuationFunction[B] {
          def apply(alternative: B): Numeraire = {
            fa(f(alternative))
          }
        }
      }
    }
  }

  def min[A]: Monoid[ValuationFunction[A]] = {
    makeMonoid(0L, _ min _)
  }

  def prod[A]: Monoid[ValuationFunction[A]] = {
    makeMonoid(1L, _ * _)
  }

  def sum[A]: Monoid[ValuationFunction[A]] = {
    makeMonoid(0L, _ + _)
  }

  private def makeMonoid[A](id: Numeraire, op: (Numeraire, Numeraire) => Numeraire) = {
    new Monoid[ValuationFunction[A]] {
      def combine(v1: ValuationFunction[A], v2: ValuationFunction[A]): ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            op(v1(a), v2(a))
          }
        }
      }
      def empty: ValuationFunction[A] = {
        new ValuationFunction[A] {
          def apply(a: A): Numeraire = {
            id
          }
        }
      }
    }
  }
}
