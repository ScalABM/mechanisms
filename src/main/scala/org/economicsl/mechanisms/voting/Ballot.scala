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
package org.economicsl.mechanisms.voting

import java.util.UUID
import org.economicsl.mechanisms.{Alternative, Preference}


case class Ballot(signature: UUID, alternative: Alternative)
  extends Preference[Alternative] {

  /** Return an integer whose sign communicates how `a1` compares to `a2`.
    *
    * The result sign has the following meaning:
    * - negative if `a2` is preferred to `a1`.
    * - positive if `a1` is weakly preferred to `a2`.
    * - zero if indifferent between `a1` and `a2`
    */
  def compare(a1: Alternative, a2: Alternative): Int = {
    if (a1.uuid != alternative.uuid && a2.uuid == alternative.uuid) {
      -1
    } else if (a1.uuid == alternative.uuid && a2.uuid != alternative.uuid) {
      1
    } else {
      0
    }
  }

}
