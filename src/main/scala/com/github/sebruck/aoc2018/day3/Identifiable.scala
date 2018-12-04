package com.github.sebruck.aoc2018.day3

trait Identifiable[T] {
  def id(x: T): Int
}
