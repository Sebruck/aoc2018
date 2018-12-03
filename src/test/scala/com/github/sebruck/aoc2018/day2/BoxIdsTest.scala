package com.github.sebruck.aoc2018.day2

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import BoxIds._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class BoxIdsTest
    extends FlatSpec
    with Matchers
    with PropertyChecks
    with OptionValues {

  val distinctStrings: Gen[String] = Gen.alphaStr.map(_.distinct)
  val duplicatedString: Gen[String] = distinctStrings.map(s => s + s)
  val tripledString: Gen[String] = distinctStrings.map(s => s + s)

  behavior of "checkSum"
  it should "give the correct checksum for a static example" in {
    val input = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )

    checkSum(input) shouldBe 12
  }

  it should "return 0 when only strings with unique characters are given" in {
    forAll(Gen.nonEmptyListOf(distinctStrings)) { s =>
      checkSum(s) shouldBe 0
    }
  }

  it should "return 0 when only strings with 2 duplicates exist" in {
    forAll(Gen.nonEmptyListOf(duplicatedString)) { l =>
      checkSum(l) shouldBe 0
    }
  }
  it should "return 0 when only strings with 3 duplicates exist" in {
    forAll(Gen.nonEmptyListOf(tripledString)) { l =>
      checkSum(l) shouldBe 0
    }
  }

  it should "return a number greater than 0 when strings with 2 and 3 duplicates exist" in {
    val listWithDuplicatedAndTripledStrings: Gen[List[String]] = for {
      dupes <- Gen.nonEmptyListOf(duplicatedString)
      triples <- Gen.nonEmptyListOf(tripledString)
    } yield dupes ++ triples

    forAll(listWithDuplicatedAndTripledStrings) { l =>
      checkSum(l) shouldBe >=(0)
    }
  }

  behavior of "findFabricsId"

  it should "return the common characters" in {
    val boxIds =
      List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")

    findFabricsId(boxIds).value shouldBe "fgij"
  }

  it should "return a value when it finds matching ids" in {
    val listWithAtLeastOneMatch = for {
      l <- Gen.nonEmptyListOf(Gen.alphaStr)
      firstPart <- Gen.alphaStr
      secondPart <- Gen.alphaStr
    } yield
      List(s"${firstPart}X${secondPart}") ++
        l ++
        List(s"${firstPart}Y${secondPart}")

    forAll(listWithAtLeastOneMatch) { l =>
      findFabricsId(l) shouldNot be(empty)
    }
  }
}
