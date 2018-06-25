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


/** Base trait representing an agent's preferences defined over a particular
  * type of `Alternative`.
  * @tparam A the sub-type of `Alternative` over which the preferences should be
  *         defined. Note that `Preference` is contra-variant in the type
  *         parameter.
  */
trait Preference[-A <: Alternative] {
  self =>

  /** Return an integer whose sign communicates how `a1` compares to `a2`.
    *
    * The result sign has the following meaning:
    * - negative if `a2` is preferred to `a1`.
    * - positive if `a1` is weakly preferred to `a2`.
    * - zero if indifferent between `a1` and `a2`
    */
  def compare(a1: A, a2: A): Int

  def ordering[A1 <: A]: Ordering[A1] = {
    new Ordering[A1] {
      def compare(a1: A1, a2: A1): Int = {
        self.compare(a1, a2)
      }
    }
  }

  /** Return `a1` if `a1` is weakly preferred to `a2`; otherwise `a2`. */
  def prefers[A1 <: A](a1: A1, a2: A1): A1 = {
    if (compare(a1, a2) >= 0) a1 else a2
  }

}
