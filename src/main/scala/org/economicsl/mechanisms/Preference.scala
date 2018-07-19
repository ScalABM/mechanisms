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

import scala.collection.immutable.TreeSet


/** Base trait representing an agent's preferences defined over a particular
  * type of `Alternative`.
  * @tparam A
  */
trait Preference[A] extends Order[A] {

  final def mostPreferred(alternatives: Iterable[A]): A = {
    alternatives.reduce(max)
  }

  final def rank(alternatives: Iterable[A]): Map[A, Int] = {
    val sortedAlternatives = TreeSet.empty[A](toOrdering) ++ alternatives
    sortedAlternatives.zipWithIndex.aggregate(Map.empty[A, Int])(_ + _, _ |+| _)
  }

}


object Preference {

  implicit val contravariant: Contravariant[Preference] = {
    new Contravariant[Preference] {
      def contramap[A, B](fa: Preference[A])(f: B => A): Preference[B] = {
        new Preference[B] {
          def compare(b1: B, b2: B): Int = {
            fa.compare(f(b1), f(b2))
          }
        }
      }
    }
  }

  def contravariantSemigroupal(implicit ev: ContravariantSemigroupal[({ type F[A] = (A, A) => Int })#F]): ContravariantSemigroupal[Preference] = {
    new ContravariantSemigroupal[Preference] {
      def contramap[A, B](fa: Preference[A])(f: B => A): Preference[B] = {
        from(ev.contramap(fa.compare)(f))
      }
      def product[A, B](fa: Preference[A], fb: Preference[B]): Preference[(A, B)] = {
        from(ev.product(fa.compare, fb.compare))
      }
    }
  }

  def from[A](f: (A, A) => Int): Preference[A] = {
    new Preference[A] {
      def compare(a1: A, a2: A): Int = {
        f(a1, a2)
      }
    }
  }

  /** Assumes that alternatives are ordered from least to most preferred. */
  def fromSeq[A](alternatives: Seq[A]): Preference[A] = {
    new Preference[A] {
      def compare(a1: A, a2: A): Int = {
        alternatives.indexOf(a1).compare(alternatives.indexOf(a2))
      }
    }
  }

  def indifference[A]: Preference[A] = {
    new Preference[A] {
      def compare(a1: A, a2: A): Int = {
        0
      }
    }
  }

  val leftBiasedContravariantSemigroupal: ContravariantSemigroupal[Preference] = {
    contravariantSemigroupal(_leftBiasedContravariantSemigroupal)
  }

  def leftBiasedMonoid[A]: Monoid[Preference[A]] = {
    monoid(_leftBiased)
  }

  def monoid[A](implicit ev: Monoid[(A, A) => Int]): Monoid[Preference[A]] = {
    new Monoid[Preference[A]] {
      def combine(p1: Preference[A], p2: Preference[A]): Preference[A] = {
        new Preference[A] {
          def compare(a1: A, a2: A): Int = {
            ev.combine(p1.compare, p2.compare)(a1, a2)
          }
        }
      }
      val empty: Preference[A] = {
        from(ev.empty)
      }
    }
  }

  /** Defines a preference for a particular alternative. */
  def particular[A](alternative: A): Preference[A] = {
    new Preference[A] {
      def compare(a1: A, a2: A): Int = {
        if ((a1 != alternative) & (a2 == alternative)) {
          -1
        } else if ((a1 == alternative) & (a2 != alternative)) {
          1
        } else {
          0
        }
      }
    }
  }

  val rightBiasedContravariantSemigroupal: ContravariantSemigroupal[Preference] = {
    contravariantSemigroupal(_rightBiasedContravariantSemigroupal)
  }

  /** Return a `Monoid[Preference[A]]` instance that combines two `Preference[A]`
    * instances to create a new `Preference[A]` instance that compares using the
    * second preference instance and then uses the first preference instance to
    * break ties.
    */
  def rightBiasedMonoid[A]: Monoid[Preference[A]] = {
    monoid(_rightBiased)
  }

  /** Derives a `Semigroup[Preference[A]]` instance from `Semigroup[(A, A) => Int]` instance. */
  def semigroup[A](implicit ev: Semigroup[(A, A) => Int]): Semigroup[Preference[A]] = {
    new Semigroup[Preference[A]] {
      def combine(p1: Preference[A], p2: Preference[A]): Preference[A] = {
        new Preference[A] {
          def compare(a1: A, a2: A): Int = {
            ev.combine(p1.compare, p2.compare)(a1, a2)
          }
        }
      }
    }
  }

  private def _leftBiased[A]: Monoid[(A, A) => Int] = {
    new Monoid[(A, A) => Int] {
      def combine(c1: (A, A) => Int, c2: (A, A) => Int): (A, A) => Int = {
        (a1, a2) => if (c1(a1, a2) == 0) c2(a1, a2) else c1(a1, a2)
      }
      def empty: (A, A) => Int = {
        (a1, a2) => 0
      }
    }
  }

  private lazy val _leftBiasedContravariantSemigroupal: ContravariantSemigroupal[({ type F[A] = (A, A) => Int })#F] = {
    new ContravariantSemigroupal[({ type F[A] = (A, A) => Int })#F] {
      def contramap[A, B](fa: (A, A) => Int)(f: B => A): (B, B) => Int = {
        (b1, b2) => fa(f(b1), f(b2))
      }
      def product[A, B](fa: (A, A) => Int, fb: (B, B) => Int): ((A, B), (A, B)) => Int = {
        (ab1: (A, B), ab2: (A, B)) =>
          if (fa(ab1._1, ab2._1) == 0) {
            fb(ab1._2, ab2._2)
          } else {
            fa(ab1._1, ab2._1)
          }
      }
    }
  }

  private def _rightBiased[A]: Monoid[(A, A) => Int] = {
    new Monoid[(A, A) => Int] {
      def combine(c1: (A, A) => Int, c2: (A, A) => Int): (A, A) => Int = {
        (a1, a2) => if (c2(a1, a2) == 0) c1(a1, a2) else c2(a1, a2)
      }
      def empty: (A, A) => Int = {
        (a1, a2) => 0
      }
    }
  }

  private lazy val _rightBiasedContravariantSemigroupal: ContravariantSemigroupal[({ type F[A] = (A, A) => Int })#F] = {
    new ContravariantSemigroupal[({ type F[A] = (A, A) => Int })#F] {
      def contramap[A, B](fa: (A, A) => Int)(f: B => A): (B, B) => Int = {
        (b1, b2) => fa(f(b1), f(b2))
      }
      def product[A, B](fa: (A, A) => Int, fb: (B, B) => Int): ((A, B), (A, B)) => Int = {
        (ab1: (A, B), ab2: (A, B)) =>
          if (fb(ab1._2, ab2._2) == 0) {
            fa(ab1._1, ab2._1)
          } else {
            fb(ab1._2, ab2._2)
          }
      }
    }
  }

}
