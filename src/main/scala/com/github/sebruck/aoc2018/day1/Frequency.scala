package com.github.sebruck.aoc2018.day1

import cats.Foldable
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.monoid._
import com.github.sebruck.aoc2018.util.Input

import scala.collection.GenTraversableOnce

object Frequency {
  def calculate[F[_]: Foldable, A: Monoid](l: F[A]): A = l.fold

  def firstDuplicatedFrequency[F[_]: Foldable, A: Monoid](
      frequencyChanges: F[A])(
      implicit asTraversable: F[A] => GenTraversableOnce[A]): A = {
    import cats.instances.stream._
    import cats.instances.either._

    Stream
      .continually(frequencyChanges)
      .flatten
      .foldM[Either[A, ?], (Set[A], A)]((Set.empty[A], Monoid.empty[A])) {
        case ((results, currentFrequency), currentChange) =>
          val nextFrequency = currentFrequency |+| currentChange
          if (results.contains(nextFrequency))
            Left(nextFrequency)
          else
            Right(results + nextFrequency, nextFrequency)
      }
      .left
      .get
  }
}

object ExecuteFrequency extends App {
  import cats.instances.list._
  import cats.instances.int._

  val input = Input.load("day1.txt").map(_.toInt)
  println(Frequency.calculate(input))
  println(Frequency.firstDuplicatedFrequency(input))
}
