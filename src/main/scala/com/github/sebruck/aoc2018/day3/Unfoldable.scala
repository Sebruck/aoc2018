package com.github.sebruck.aoc2018.day3

trait Unfoldable[T, A] {
  def unfold(x: T): List[A]
}
