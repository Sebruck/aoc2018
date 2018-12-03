package com.github.sebruck.aoc2018.day3

import com.github.sebruck.aoc2018.day3
import com.github.sebruck.aoc2018.util.Input

import scala.util.Try

case class Coordinates(x: Int, y: Int)
object Coordinates {
  def apply(stringRepresentation: String): Try[Coordinates] = Try {
    val parts = stringRepresentation.split(",")
    Coordinates(parts(0).toInt, parts(1).toInt)
  }
}

case class Dimensions(width: Int, height: Int)
object Dimensions {
  def apply(stringRepresentation: String): Try[Dimensions] = Try {
    val parts = stringRepresentation.split("x")
    Dimensions(parts(0).toInt, parts(1).toInt)
  }
}

case class Claim(topLeft: Coordinates, dimensions: Dimensions)

object Rectangles {

  def countOverlaps(claims: List[Claim]): Int = {
    def getPoints(claim: Claim): Seq[Coordinates] =
      for {
        addX <- 1 to claim.dimensions.width
        addY <- 1 to claim.dimensions.height
      } yield
        claim.topLeft
          .copy(x = claim.topLeft.x + addX, y = claim.topLeft.y + addY)

    claims
      .flatMap(getPoints)
      .groupBy(identity)
      .values
      .count(_.size > 1)
  }
}

object RunRectangles extends App {
  val claims = Input
    .load("day3.txt")
    .map { line =>
      val box = line.split("@")(1)
      val parts = box.split(":")
      Claim(Coordinates(parts(0).trim).get, Dimensions(parts(1).trim).get)
    }

  println(Rectangles.countOverlaps(claims))
}
