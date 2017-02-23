import java.util.UUID

import org.economicsl.mechanisms.{Alternative, Preference, SocialChoiceFunction}

import collection.{GenMap, GenSet, parallel}


case class Ballot(signature: UUID, alternative: Alternative) extends Preference[Alternative] {

  // Insures that the head of any sorted collection of alternatives is the preferred alternative!
  val ordering: Ordering[Alternative] = Ordering.by(a => if (a == alternative) 0 else 1)

}


type VoteCounts = GenMap[Alternative, Int]


class PluralityVotingRule(initial: VoteCounts) extends SocialChoiceFunction[Alternative, Ballot] {

  def apply(ballots: GenSet[Ballot]): Alternative = {
    val results = ballots.aggregate(initial)((counts, ballot) => update(counts, ballot), (m1, m2) => combine(m1, m2))
    val (winner, _) = results maxBy { case (_, votes) => votes }  // todo explicitly handle ties!
    winner
  }

  private[this] def update(current: VoteCounts, ballot: Ballot): VoteCounts = {
    val votes = current.getOrElse(ballot.alternative, 0)
    current.updated(ballot.alternative, votes + 1)
  }

  private[this] def combine(some: VoteCounts, other: VoteCounts): VoteCounts = {
    some.foldLeft(other){ case (counts, (candidate, votes)) =>
      val additionalVotes = other.getOrElse(candidate, 0)
      counts.updated(candidate, votes + additionalVotes)
    }
  }

}


// Quickly check that mechanism is working properly
val winner = new Alternative {}
val ballots = Set(Ballot(UUID.randomUUID(), new Alternative {}), Ballot(UUID.randomUUID(), winner), Ballot(UUID.randomUUID(), winner))
val votingRule = new PluralityVotingRule(Map.empty[Alternative, Int])
assert(winner == votingRule(ballots))

// to use parallel mechanism simply use parallel collections as input!
val parBallots = parallel.ParSet(Ballot(UUID.randomUUID(), new Alternative {}), Ballot(UUID.randomUUID(), winner), Ballot(UUID.randomUUID(), winner))
val parVotingRule = new PluralityVotingRule(parallel.ParMap.empty[Alternative, Int])
assert(winner == votingRule(parBallots))



