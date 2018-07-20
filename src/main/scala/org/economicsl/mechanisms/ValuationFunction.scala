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

  def contravariantSemigroupal(implicit ev: ContravariantSemigroupal[({ type F[A] = A => Numeraire })#F]): ContravariantSemigroupal[ValuationFunction] = {
    new ContravariantSemigroupal[ValuationFunction] {
      def contramap[A, B](fa: ValuationFunction[A])(f: B => A): ValuationFunction[B] = {
        from(ev.contramap(fa.apply)(f))
      }
      def product[A, B](fa: ValuationFunction[A], fb: ValuationFunction[B]): ValuationFunction[(A, B)] = {
        from(ev.product(fa.apply, fb.apply))
      }
    }
  }

  def from[A](v: A => Numeraire): ValuationFunction[A] = {
    new ValuationFunction[A] {
      def apply(alternative: A): Numeraire = {
        v(alternative)
      }
    }
  }

  def min[A]: Monoid[ValuationFunction[A]] = {
    monoid(_minMonoid)
  }

  def prod[A]: Monoid[ValuationFunction[A]] = {
    monoid(_prodMonoid)
  }

  def sum[A]: Monoid[ValuationFunction[A]] = {
    monoid(_sumMonoid)
  }

  def monoid[A](implicit ev: Monoid[A => Numeraire]): Monoid[ValuationFunction[A]] = {
    new Monoid[ValuationFunction[A]] {
      def combine(v1: ValuationFunction[A], v2: ValuationFunction[A]): ValuationFunction[A] = {
        from(ev.combine(v1, v2))
      }
      def empty: ValuationFunction[A] = {
        from(ev.empty)
      }
    }
  }

  def particular[A](alernative: A)(implicit ev: ValuationFunction[A]): ValuationFunction[A] = {
    new Valuation[A] {
      def apply(a: A): Numeraire = {
        if (a == alternative) ev(a) else 0L
      }
    }
  }

  def semigroup[A](implicit ev: Semigroup[A => Numeraire]): Semigroup[ValuationFunction[A]] = {
    new Semigroup[ValuationFunction[A]] {
      def combine(v1: ValuationFunction[A], v2: ValuationFunction[A]): ValuationFunction[A] = {
        from(ev.combine(v1, v2))
      }
    }
  }

  private def _minMonoid[A] = {
    new Monoid[A => Numeraire] {
      def combine(v1: A => Numeraire, v2: A => Numeraire): A => Numeraire = {
        a => v1(a).min(v2(a))
      }
      def empty: A => Numeraire = {
        a => Long.MaxValue
      }
    }
  }

  private def _prodMonoid[A] = {
    new Monoid[A => Numeraire] {
      def combine(v1: A => Numeraire, v2: A => Numeraire): A => Numeraire = {
        a => v1(a) * v2(a)
      }
      def empty: A => Numeraire = {
        a => 1L
      }
    }
  }

  private def _sumMonoid[A] = {
    new Monoid[A => Numeraire] {
      def combine(v1: A => Numeraire, v2: A => Numeraire): A => Numeraire = {
        a => v1(a) + v2(a)
      }
      def empty: A => Numeraire = {
        a => 0L
      }
    }
  }

}
