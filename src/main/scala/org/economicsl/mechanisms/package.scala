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
package org.economicsl

import scala.collection.GenSeq


package object mechanisms {

  /** Type representing "money".
    *
    * When modeling individual agent preferences using a `Preference` ordering,
    * we are not modeling "by how much" an agent prefers one alternative over
    * another. Introducing the concept of "money" provides a yardstick that allows us
    * to model exactly this idea.  A key property of "money" is that it is
    * capable of acting as a store of value that can be transferred amongst
    * groups of agents.
    */
  type Numeraire = Long

  /** Type representing an indexed sequence of payments. */
  type Payments = GenSeq[Numeraire]

  /** Type representing an indexed sequence of payment functions. */
  type PaymentFunctions[A <: Alternative] = GenSeq[PaymentFunction[ValuationFunctions[A]]]

  /** Type representing an indexed sequence of valuation functions. */
  type ValuationFunctions[A <: Alternative] = GenSeq[ValuationFunction[A]]

}
