/*
Copyright 2018 EconomicSL
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
package org.economicsl.mechanisms.voting

import java.util.UUID

import org.economicsl.mechanisms.Preference
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

import scala.util.{Failure, Success}


object VotingRuleSpecification extends Properties("voting-rules") {

  /** Generates a collection of A. */
  def alternatives[A](g: Gen[A]): Gen[Vector[A]] = {
    Gen.nonEmptyContainerOf[Vector, A](g)
  }

  /** Generates a preference for a particular A. */
  def particular[A](g: Gen[A]): Gen[Preference[A]] = {
    g.map(a => Preference.particular(a))
  }

  /*** Generates a collection of preferences for the same alternative. */
  def unanimous[A](alternative: A): Gen[Vector[Preference[A]]] = {
    Gen.nonEmptyContainerOf[Vector, Preference[A]](particular(Gen.const(alternative)))
  }

  def inputData[A](g: Gen[A]): Gen[(Vector[A], A, Vector[Preference[A]])] = {
    for {
      as <- alternatives(g)
      a <- Gen.oneOf(as)
      ps <- unanimous(a)
    } yield (as, a, ps)
  }

  case class Candidate(uuid: UUID)

  val candidate: Gen[Candidate] = {
    Gen.uuid.map(uuid => Candidate(uuid))
  }

  property("borda count satisfies unanimity") = Prop.forAll(inputData(candidate)) {
    case (candidates, candidate, preferences) =>
      val winner = VotingRule.bordaCount(preferences)(candidates)
      winner == candidate
  }

  property("majority satisfies unanimity") = Prop.forAll(inputData(Gen.option(candidate))) {
    case (candidates, candidate, preferences) =>
      val winner = VotingRule.majority(preferences)(candidates)
      winner == candidate
  }

  property("plurality satisfies unanimity") = Prop.forAll(inputData(candidate)) {
    case (candidates, candidate, preferences) =>
      val winner = VotingRule.plurality(preferences)(candidates)
      winner == candidate
  }

}
