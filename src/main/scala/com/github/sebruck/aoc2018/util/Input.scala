package com.github.sebruck.aoc2018.util

import scala.io.Source

object Input {
  def load(filename: String): List[String] =
    Source.fromResource(filename).getLines().toList
}
