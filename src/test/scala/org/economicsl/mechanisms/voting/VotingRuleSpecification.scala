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

  case class Candidate(uuid: UUID, name: String)

  case class Voter(uuid: UUID, preference: Preference[Candidate])

  val randomCandidate: Gen[Candidate] = {
    for {
      uuid <- Gen.uuid
      name <- Arbitrary.arbitrary[String]
    } yield Candidate(uuid, name)
  }

  def randomCandidates: Gen[Vector[Candidate]] = {
    Gen.nonEmptyContainerOf[Vector, Candidate](randomCandidate)
  }

  def randomParticularVoter(candidate: Candidate): Gen[Voter] = {
    for {
      uuid <- Gen.uuid
    } yield Voter(uuid, Preference.particular(candidate))
  }

  def randomParticularVoters(candidate: Candidate): Gen[Vector[Voter]] = {
    Gen.nonEmptyContainerOf[Vector, Voter](randomParticularVoter(candidate))
  }


  val randomInputData: Gen[(Vector[Candidate], Candidate, Vector[Voter])] = {
    for {
      cs <- randomCandidates
      c <- Gen.oneOf[Candidate](cs)
      vs <- randomParticularVoters(c)
    } yield (cs, c, vs)
  }

  property("borda count satisfies unanimity") = Prop.forAll(randomInputData) {
    case (candidates, candidate, voters) =>
      val winner = VotingRule.bordaCount(voters.map(_.preference))(candidates)
      winner == candidate
  }

  property("plurality satisfies unanimity") = Prop.forAll(randomInputData) {
    case (candidates, candidate, voters) =>
      val winner = VotingRule.plurality(voters.map(_.preference))(candidates)
      winner == candidate
  }

}
