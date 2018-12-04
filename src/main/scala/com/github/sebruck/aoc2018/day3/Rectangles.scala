package com.github.sebruck.aoc2018.day3

import cats.syntax.foldable._
import com.github.sebruck.aoc2018.util.Input

import scala.util.Try

object Rectangles {

  def countOverlaps[A, T](elements: List[T])(
      implicit uf: Unfoldable[T, A]): Int = {
    import cats.instances.int._
    import cats.instances.list._
    import cats.instances.map._

    elements
      .flatMap(element => uf.unfold(element))
      .foldMap(unfolded => Map(unfolded -> 1))
      .values
      .count(_ > 1)
  }

  def withoutOverlap[A, T](elements: List[T])(
      implicit uf: Unfoldable[T, A],
      id: Identifiable[T]): Option[T] = {

    def noOverlaps(element1: T)(element2: T): Boolean =
      id.id(element1) == id.id(element2) || uf
        .unfold(element1)
        .toSet
        .intersect(uf.unfold(element2).toSet)
        .isEmpty

    elements.find(element => elements.forall(noOverlaps(element)))
  }
}

object RunRectangles extends App {
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
  case class Claim(id: Int, topLeft: Coordinates, dimensions: Dimensions)

  implicit object ClaimTypeClassInstance
      extends Unfoldable[Claim, Coordinates]
      with Identifiable[Claim] {
    override def id(claim: Claim): Int = claim.id

    override def unfold(claim: Claim): List[Coordinates] =
      (for {
        addX <- 1 to claim.dimensions.width
        addY <- 1 to claim.dimensions.height
      } yield
        claim.topLeft
          .copy(x = claim.topLeft.x + addX, y = claim.topLeft.y + addY)).toList
  }

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
