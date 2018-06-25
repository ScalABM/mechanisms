import java.util.UUID

import org.economicsl.mechanisms.{Alternative, Preference, SocialChoiceFunction}

import collection.{GenMap, GenSet, parallel}

// Quickly check that mechanism is working properly
val winner = new Alternative {}
val ballots = Set(Ballot(UUID.randomUUID(), new Alternative {}), Ballot(UUID.randomUUID(), winner), Ballot(UUID.randomUUID(), winner))
val votingRule = new PluralityVotingRule(Map.empty[Alternative, Int])
assert(winner == votingRule(ballots))

// to use parallel mechanism simply use parallel collections as input!
val parBallots = parallel.ParSet(Ballot(UUID.randomUUID(), new Alternative {}), Ballot(UUID.randomUUID(), winner), Ballot(UUID.randomUUID(), winner))
val parVotingRule = new PluralityVotingRule(parallel.ParMap.empty[Alternative, Int])
assert(winner == votingRule(parBallots))
