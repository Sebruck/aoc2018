package com.github.sebruck.aoc2018.day5

import com.github.sebruck.aoc2018.util.Input

object Polymer {
  def react(units: String): String =
    units
      .foldLeft("")(
        (acc, current) =>
          acc.headOption
            .map(last =>
              if (current != last && current.toLower == last.toLower) acc.tail
              else current + acc)
            .getOrElse(current + acc)
      )
      .reverse

  def findShortest(units: String): String = {
    ('a' to 'z')
      .map(unitType =>
        units.replaceAll(s"[${unitType}${unitType.toUpper}]", ""))
      .map(react)
      .minBy(_.length)
  }
}

object RunPolymer extends App {
  val input = Input.load("day5.txt").mkString

  println(Polymer.react(input).length)
  println(Polymer.findShortest(input).length)
}
