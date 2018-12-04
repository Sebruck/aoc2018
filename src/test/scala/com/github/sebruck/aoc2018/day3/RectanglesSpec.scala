package com.github.sebruck.aoc2018.day3

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import Rectangles._

class RectanglesSpec extends FlatSpec with Matchers with OptionValues {

  case class TestElement(id: Int, x: List[Int])
  implicit object TestTypeClasses
      extends Unfoldable[TestElement, Int]
      with Identifiable[TestElement] {
    override def unfold(x: TestElement): List[Int] = x.x
    override def id(x: TestElement): Int = x.id
  }

  "countOverlaps" should "return the correct amount of overlaps" in {
    val testInput = List(
      TestElement(1, List(1, 2, 3, 4)),
      TestElement(2, List(2, 3, 5, 6)),
      TestElement(3, List(6, 7, 8))
    )
    countOverlaps(testInput) shouldBe 3
  }

  "countOverlaps" should "0 on empty input" in {
    countOverlaps(List.empty) shouldBe 0
  }

  "withoutOverlap" should "return the element without an overlap" in {
    val testInput = List(
      TestElement(1, List(1, 2, 3, 4)),
      TestElement(2, List(2, 3, 5, 6)),
      TestElement(3, List(7, 8, 9))
    )
    withoutOverlap(testInput).value.id shouldBe 3
  }

  "withoutOverlap" should "return None when all elements have overlaps" in {
    val testInput = List(
      TestElement(1, List(1, 2, 3, 4)),
      TestElement(2, List(2, 3, 5, 6))
    )
    withoutOverlap(testInput) shouldBe empty
  }
}
