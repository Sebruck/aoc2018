package com.github.sebruck.aoc2018.day2

import com.github.sebruck.aoc2018.util.Input

object BoxIds {

  def checkSum(ids: List[String]): Int = {
    def hasSameChars(n: Int)(id: String): Boolean =
      id.groupBy(identity).values.exists(_.length == n)

    def countSameChars(n: Int): Int =
      ids.count(hasSameChars(n))

    countSameChars(2) * countSameChars(3)
  }

  def findFabricsId(ids: List[String]): Option[String] = {
    import cats.syntax.foldable._
    import cats.instances.list._
    import cats.instances.either._

    def getSameCharacters(id1: String)(id2: String): String =
      id1.zip(id2).filter(chars => chars._1 == chars._2).map(_._1).mkString

    def hasOneDifferent(id1: String)(id2: String): Boolean =
      getSameCharacters(id1)(id2).length == id1.length - 1

    /* idsToCheckOn optimises the speed of this algorithm. Each id we already checked and did not
     find a match for, we do not have to check again with all the other ids. */
    ids
      .foldM[Either[String, ?], List[String]](ids.tail) { (idsToCheckOn, id) =>
        idsToCheckOn
          .find(hasOneDifferent(id))
          .map(getSameCharacters(id))
          .toLeft(idsToCheckOn.tail)
      }
      .left
      .toOption
  }
}

object RunBoxIds extends App {
  val input = Input.load("day2.txt")
  println(BoxIds.checkSum(input))
  println(BoxIds.findFabricsId(input))
}
