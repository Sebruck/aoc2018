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

case class Claim(id: Int, topLeft: Coordinates, dimensions: Dimensions) {
  val allPoints: Set[Coordinates] =
    (for {
      addX <- 1 to dimensions.width
      addY <- 1 to dimensions.height
    } yield
      topLeft
        .copy(x = topLeft.x + addX, y = topLeft.y + addY)).toSet
}

object Rectangles {

  def countOverlaps(claims: List[Claim]): Int = {
    claims
      .flatMap(_.allPoints)
      .groupBy(identity)
      .values
      .count(_.size > 1)
  }

  def withoutOverlap(claims: List[Claim]): Option[Claim] = {
    def noOverlaps(claim1: Claim)(claim2: Claim): Boolean =
      claim1.allPoints.intersect(claim2.allPoints).isEmpty

    claims.find(claim =>
      claims.forall(otherClaim =>
        claim.id == otherClaim.id || noOverlaps(claim)(otherClaim)))
  }
}

object RunRectangles extends App {
  val claims = Input
    .load("day3.txt")
    .map { line =>
      val Array(id, box) = line.split("@")
      val parts = box.split(":")
      Claim(id.trim.substring(1).toInt,
            Coordinates(parts(0).trim).get,
            Dimensions(parts(1).trim).get)
    }

  println(Rectangles.countOverlaps(claims))
  println(Rectangles.withoutOverlap(claims))
}
