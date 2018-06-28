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

import scala.collection.GenSeq


/** A Direct Revelation Mechanism combines a social choice function` with a
  * collection of payment functions.
  * @note See definition 9.14 from ''Algorithmic Game Theory'' for details.
  */
trait DirectRevelationMechanism[-CC1 <: GenSeq[ValuationFunction[A]],
                                -CC2 <: GenSeq[PaymentFunction[CC1]],
                                A <: Alternative,
                                CC3 <: GenSeq[Numeraire]]
  extends SocialChoiceFunction[CC1, A]
  with Function2[CC1, CC2, (A, CC3)]
