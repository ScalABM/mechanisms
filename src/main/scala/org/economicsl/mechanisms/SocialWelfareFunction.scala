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

/** Base trait defining a generic social welfare function.
  *
  * A social welfare function aggregates the preferences of individual agents
  * into a common preference ordering.
  */
trait SocialWelfareFunction[-CC <: Iterable[P], +P <: Preference[A], A]
  extends (CC => P)


/** Companion object for the `SocialWelFareFunction` trait. */
object SocialWelfareFunction {

  val vectorInvariant: Invariant[({ type F[A] = SocialWelfareFunction[Vector[Preference[A]], Preference[A], A] })#F] = {
    invariant[Vector]
  }

  val setInvariant: Invariant[({ type F[A] = SocialWelfareFunction[Set[Preference[A]], Preference[A], A] })#F] = {
    new Invariant[({ type F[A] = SocialWelfareFunction[Set[Preference[A]], Preference[A], A] })#F] {
      def imap[A, B](fa: SocialWelfareFunction[Set[Preference[A]], Preference[A], A])(f: A => B)(g: B => A): SocialWelfareFunction[Set[Preference[B]], Preference[B], B] = {
        new SocialWelfareFunction[Set[Preference[B]], Preference[B], B] {
          def apply(preferences: Set[Preference[B]]): Preference[B] = {
            fa(preferences.map(pb => pb.contramap(f))).contramap(g)
          }
        }
      }
    }
  }

  val seqInvariant: Invariant[({ type F[A] = SocialWelfareFunction[Seq[Preference[A]], Preference[A], A] })#F] = {
    new Invariant[({ type F[A] = SocialWelfareFunction[Seq[Preference[A]], Preference[A], A] })#F] {
      def imap[A, B](fa: SocialWelfareFunction[Seq[Preference[A]], Preference[A], A])(f: A => B)(g: B => A): SocialWelfareFunction[Seq[Preference[B]], Preference[B], B] = {
        new SocialWelfareFunction[Seq[Preference[B]], Preference[B], B] {
          def apply(preferences: Seq[Preference[B]]): Preference[B] = {
            fa(preferences.map(pb => pb.contramap(f))).contramap(g)
          }
        }
      }
    }
  }

  def invariant[C[X] <: Iterable[X] : Functor]: Invariant[({ type F[A] = SocialWelfareFunction[C[Preference[A]], Preference[A], A] })#F] = {
    new Invariant[({ type F[A] = SocialWelfareFunction[C[Preference[A]], Preference[A], A] })#F] {
      def imap[A, B](fa: SocialWelfareFunction[C[Preference[A]], Preference[A], A])(f: A => B)(g: B => A): SocialWelfareFunction[C[Preference[B]], Preference[B], B] = {
        new SocialWelfareFunction[C[Preference[B]], Preference[B], B] {
          def apply(preferences: C[Preference[B]]): Preference[B] = {
            fa(Functor[C].map(preferences)(pb => pb.contramap(f))).contramap(g)
          }
        }
      }
    }
  }

}
