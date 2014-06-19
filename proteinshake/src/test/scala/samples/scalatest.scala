/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package samples

/*
ScalaTest facilitates different styles of testing by providing traits you can mix
together to get the behavior and syntax you prefer.  A few examples are
included here.  For more information, visit:

http://www.scalatest.org/

One way to use ScalaTest is to help make JUnit or TestNG tests more
clear and concise. Here's an example:
*/
import scala.collection.mutable.Stack
import org.scalatest.Assertions
import org.junit.Test
import eu.hanefeld.ba.Types.SpinDomain

import cc.factorie.variable.CategoricalDomain
import eu.hanefeld.ba.{MutualInformationConnectionPredictor, PottsModel, MSA, Spin}
import cc.factorie.la.Tensor


/*
ScalaTest also supports the behavior-driven development style, in which you
combine tests with text that specifies the behavior being tested. Here's
an example whose text output when run looks like:

A Map
- should only contain keys and values that were added to it
- should report its size as the number of key/value pairs it contains
*/


import cc.factorie.Factorie.{Tensor1, DenseTensor1}
import org.junit.Assert.{assertEquals, assertArrayEquals}
class PottsModelCountersSuite extends Assertions {
  val domain: SpinDomain = new CategoricalDomain[Char](List('a', 'b', 'c'))
  val seq0 = Spin.makeSequence("aaaa", domain)
  val seq1 = Spin.makeSequence("bbbb", domain)
  val seq2 = Spin.makeSequence("cccc", domain)
  val seq3 = Spin.makeSequence("cccc", domain)
  val msa = new MSA(List(seq0, seq1, seq2, seq3), Set(), domain, "test")

  @Test def PottsModelFreqCounterShouldCountLocalFreqs() {
    val freqModel = PottsModel.frequenciesAsWeights(msa.sequences, msa.domain, false, pseudoCount=0)
    //                         a     b     c
    val expectedCounts = Array(0.25, 0.25, 0.5)
    def actualCounts(i: Int): Array[Double] = { freqModel.localFamilies(i).weights.value.toArray }
    assertArrayEquals(expectedCounts, actualCounts(0), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(3), 0.0001)
  }
  @Test def PottsModelFreqCounterShouldCountPairwiseFreqs() {
    val freqModel = PottsModel.frequenciesAsWeights(msa.sequences, msa.domain, false, pseudoCount=0)

    //This expectedCounts should hold true for all pairwise frequencies
    //                         a     b    c
    val expectedCounts = Array(0.25, 0.0,  0.0,  //a
                               0.0,  0.25, 0.0,  //b
                               0.0,  0.0,  0.5)  //c
    def actualCounts(i: Int, j: Int): Array[Double] = { freqModel.pairwiseFamilies((i, j)).weights.value.toArray }

    assertArrayEquals(expectedCounts, actualCounts(0, 1), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(0, 2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(0, 3), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1, 2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1, 3), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(2, 3), 0.0001)
  }
  @Test def PottsModelFreqCounterShouldCountLocalFreqsWithPseudocount() {
    val freqModel = PottsModel.frequenciesAsWeights(msa.sequences, msa.domain, false, pseudoCount=1)
    //we now have 4 actual sequences (M) and 3 fake observations (domain.size*pseudoCount) for each element in the domain
    val expectedCounts = Array( 2.0 / 7, 2.0 / 7, 3.0 / 7)
    def actualCounts(i: Int): Array[Double] = { freqModel.localFamilies(i).weights.value.toArray }
    assertArrayEquals(expectedCounts, actualCounts(0), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(3), 0.0001)

  }
  @Test def PottsModelFreqCounterShouldCountPairwiseFreqsWithPseudocount() {
    val freqModel = PottsModel.frequenciesAsWeights(msa.sequences, msa.domain, false, pseudoCount=1)

    //The pseudocount does not mean that every possible pair is observed.
    //Instead we add pseudoCount/domain.size "oberservations" for each possible pair, and retain de denominator from above (4+3)
    //This ensures consitency for (sum_j f_ij == fi)

    val fractObs = 1. / domain.size
    //This expectedCounts should hold true for all pairwise frequencies

    val expectedCounts = Array(
    // a                      b                      c
      (1.0 + fractObs) / 7,  (0.0 + fractObs) / 7,  (0.0 + fractObs) / 7,  //a
      (0.0 + fractObs) / 7,  (1.0 + fractObs) / 7,  (0.0 + fractObs) / 7,  //b
      (0.0 + fractObs) / 7,  (0.0 + fractObs) / 7,  (2.0 + fractObs) / 7)  //c
    def actualCounts(i: Int, j: Int): Array[Double] = { freqModel.pairwiseFamilies((i, j)).weights.value.toArray }

    assertArrayEquals(expectedCounts, actualCounts(0, 1), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(0, 2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(0, 3), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1, 2), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(1, 3), 0.0001)
    assertArrayEquals(expectedCounts, actualCounts(2, 3), 0.0001)
  }
}

class MutualInformationSuite extends Assertions {

  val domain: SpinDomain = new CategoricalDomain[Char](List('a', 'b'))
  val seq0 = Spin.makeSequence("aba", domain)
  val seq1 = Spin.makeSequence("bbb", domain)
  val seq2 = Spin.makeSequence("aba", domain)
  val seq3 = Spin.makeSequence("bbb", domain)
  val msa = new MSA(List(seq0, seq1, seq2, seq3), Set((0, 2)), domain, "test")
  val MIPredictor = new MutualInformationConnectionPredictor(msa, false)

  @Test def MIShouldBeCorrect() {
    val expectedStrength02 =
      (5.0/12 * math.log(20.0/12)) +
      (1.0/12 * math.log(1.0/3)) +
      (5.0/12 * math.log(20.0/12)) +
      (1.0/12 * math.log(1.0/3))

    assertEquals(0, MIPredictor.strengths((0, 1)), 0.0001)
    assertEquals(0, MIPredictor.strengths((1, 2)), 0.0001)
    assertEquals(expectedStrength02, MIPredictor.strengths((0, 2)), 0.0001)
  }

  @Test def TPRateShouldBeCorrect() {
    assert(MIPredictor.strengthsDescending(0)._1 == (0, 2))
    assert(MIPredictor.TPRate(100) == MIPredictor.TPRate(1))
    assertEquals(1.0, MIPredictor.TPRate(1), 0.0001)
  }

}

class FrobeniusNormSuite extends Assertions {

}