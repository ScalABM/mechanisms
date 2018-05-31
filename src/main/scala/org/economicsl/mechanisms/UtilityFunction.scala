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


trait UtilityFunction[-A <: Alternative]
  extends Preference[A] with ((A) => Utility) {

  def compare(a1: A, a2: A): Int = {
    ordering.compare(a1, a2)
  }

  override def ordering[A1 <: A]: Ordering[A1] = {
    Ordering.by[A1, Utility](alternative => apply(alternative))
  }

}
