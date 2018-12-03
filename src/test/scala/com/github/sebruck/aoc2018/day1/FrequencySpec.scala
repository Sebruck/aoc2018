package com.github.sebruck.aoc2018.day1

import cats.instances.int._
import cats.instances.list._
import com.github.sebruck.aoc2018.day1.Frequency._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}

class FrequencySpec extends FlatSpec with Matchers {

  behavior of "calculate"

  it should "calculate the correct frequency" in {
    val changes = List(1, -2, 3, 1)
    calculate(changes) shouldBe 3
  }

  it should "be 0 when an empty list is given" in {
    calculate(List.empty[Int]) shouldBe 0
  }

  behavior of "firstDuplicatedFrequency"

  it should "return the first duplicate frequency" in {
    firstDuplicatedFrequency(List(7, 7, -2, -7, -4)) shouldBe 14
  }
}

class FrequencyPropSpec extends PropSpec with PropertyChecks with Matchers {

  val genBalancedList: Gen[List[Int]] =
    Gen
      .nonEmptyListOf(Gen.posNum[Int])
      .map(_.toSet.toList)
      .map(l => l ++ l.map(_ * -1))

  property(
    "calculate return greater than 0 when only positive integers are given") {
    forAll(Gen.nonEmptyListOf(Gen.posNum[Int])) { l =>
      calculate(l) shouldBe >(0)
    }
  }

  property("calculate return less than 0 when only negative integers are given") {
    forAll(Gen.nonEmptyListOf(Gen.negNum[Int])) { l =>
      calculate(l) shouldBe <(0)
    }
  }

  property("calculate return 0 when giving a balanced list") {
    forAll(genBalancedList) { l =>
      calculate(l) shouldBe 0
    }
  }

  property(
    "firstDuplicatedFrequency return a non negative number giving a balanced list with positive integers first") {
    forAll(genBalancedList) { l =>
      firstDuplicatedFrequency(l) shouldBe >=(0)
    }
  }
}
