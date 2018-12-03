package com.github.sebruck.aoc2018.day3

import org.scalatest.{FlatSpec, Matchers}
import Rectangles._

class RectanglesSpec extends FlatSpec with Matchers {

  it should "return the correct amount of overlaps" in {
    val claims = List(
      Claim(Coordinates(1, 3), Dimensions(4, 4)),
      Claim(Coordinates(3, 1), Dimensions(4, 4)),
      Claim(Coordinates(5, 5), Dimensions(2, 2))
    )

    countOverlaps(claims) shouldBe 4
  }
}
