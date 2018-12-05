package com.github.sebruck.aoc2018.day5

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Polymer._

class PolymerSpec extends FlatSpec with Matchers with PropertyChecks {

  val alphaUpperAndLower = Gen
    .nonEmptyListOf(
      Gen.frequency((1, Gen.alphaUpperChar), (1, Gen.alphaLowerChar)))
    .map(_.mkString)

  "react" should "reduce the units" in {
    react("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
  }

  it should "be always smaller or equal than the initial string" in {
    forAll(alphaUpperAndLower, MinSuccessful(1000)) { s =>
      react(s).length shouldBe <=(s.length)
    }
  }

  "findShortest" should "find the shortest" in {
    findShortest("dabAcCaCBAcCcaDA") shouldBe "daDA"
  }

  it should "be always smaller than reacting without substituion" in {
    forAll(alphaUpperAndLower, MinSuccessful(1000)) { s =>
      val reacted = react(s).length
      whenever(reacted > 0) {
        findShortest(s).length shouldBe <(reacted)
      }
    }
  }
}
