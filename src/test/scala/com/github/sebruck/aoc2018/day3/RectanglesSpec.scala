package com.github.sebruck.aoc2018.day3

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import Rectangles._

class RectanglesSpec extends FlatSpec with Matchers with OptionValues {

  val claims = List(
    Claim(1, Coordinates(1, 3), Dimensions(4, 4)),
    Claim(2, Coordinates(3, 1), Dimensions(4, 4)),
    Claim(3, Coordinates(5, 5), Dimensions(2, 2))
  )

  "countOverlaps" should "return the correct amount of overlaps" in {

    countOverlaps(claims) shouldBe 4
  }

  "withoutOverlap" should "return the claim without an overlap" in {
    withoutOverlap(claims).value.id shouldBe 3
  }
}
