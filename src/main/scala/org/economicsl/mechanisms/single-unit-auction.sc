import java.util.UUID

import org.economicsl.mechanisms._

import scala.collection.{GenMap, GenSet}
import scala.util.Random


class SecondPriceAuction {

  private[this] val alternatives = ???

  private[this] val mechanism = VickreyClarkeGrovesMechanism(???)

}


/** Example 1: Auction of a single item. */
case class Outcome(winner: UUID) extends Alternative

val numberPlayers = 10
val outcomes = for (i <- 1 to numberPlayers) yield Outcome(UUID.randomUUID())

// for this example the set of alternatives is just the ser of player!
val alternatives: Set[Outcome] = outcomes.toSet

// for this example each player has a valuation of zero is he loses, and 0 < v < 1 if he wins
val prng: Random = new Random (42)
val valuations: Map[UUID, ValuationFunction[Outcome]] = alternatives.map { alternative =>
  val valuation = new ValuationFunction[Outcome] {
    def apply(outcome: Outcome): Money = if (outcome.winner == alternative.winner) _valuation else 0
    private[this] val _valuation = prng.nextDouble()
  }
  (alternative.winner, valuation)
}.toMap


def clarkePivotRule[A <: Alternative](alternatives: GenSet[A]): PaymentFunction[A] = {
  new PaymentFunction[A] {
    def apply(valuations: GenMap[UUID, ValuationFunction[A]]): Money = {
      val socialValuation = VickreyClarkeGrovesMechanism.aggregate(valuations)
      alternatives.aggregate(Double.MinValue)((maxValue, a) => maxValue max socialValuation(a), _ max _)
    }
  }
}

val hs = (for (outcome <- outcomes) yield (outcome.winner, clarkePivotRule(alternatives))).toMap
val mechanism = VickreyClarkeGrovesMechanism(hs)

val outcome = mechanism(alternatives.par, valuations.par)
val payments = mechanism.payments(outcome, valuations.par)

// winner of the auction should be the player with the highest valuation...
val (expectedWinner, _) = valuations.maxBy{case (uuid, v) => v(Outcome(uuid)) }
assert(expectedWinner == outcome.winner)

val losers = valuations - outcome.winner
val (firstLoser, secondHighestValuation) = losers.maxBy{ case(uuid, v) => v(Outcome(uuid)) }
val expectedPayment = secondHighestValuation(Outcome(firstLoser))
assert(expectedPayment == payments(outcome.winner))

// confirm ex post individually rational
val isIndividuallyRational = valuations.forall{ case (uuid, v) => v(outcome) - payments(uuid) >= 0.0 }
