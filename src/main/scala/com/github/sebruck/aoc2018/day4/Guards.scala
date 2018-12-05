package com.github.sebruck.aoc2018.day4

import com.github.sebruck.aoc2018.util.Input

object Guards {
  import cats.syntax.foldable._
  import cats.instances.list._
  import cats.instances.map._
  import cats.instances.int._

  type Minute = Int
  type GuardId = Int
  type MinutesDistribution = Map[Minute, Int]

  case class GuardStatistic(guardId: Int, mostSleepedAtMinute: Int)

  private def toSleepingMinutesDistribution(
      shifts: List[Shift]): MinutesDistribution =
    shifts.flatMap(_.sleepyMinutes.map(min => Map(min -> 1))).combineAll

  private def getMostSleepyGuard(
      allShifts: List[Shift],
      mostSleepy: MinutesDistribution => Int): Option[GuardStatistic] =
    allShifts
      .groupBy(_.guardId)
      .mapValues(toSleepingMinutesDistribution)
      .toList
      .sortBy(x => mostSleepy(x._2))(Ordering[Int].reverse)
      .headOption
      .map {
        case (guardId, sleepyMinutes) =>
          val mostSleepedAtMinute =
            sleepyMinutes.toList.sortBy(_._2)(Ordering[Int].reverse).head._1
          GuardStatistic(guardId, mostSleepedAtMinute)
      }

  def getMostSleepyGuard1(allShifts: List[Shift]): Option[GuardStatistic] =
    getMostSleepyGuard(allShifts, _.values.sum)

  def getMostSleepyGuard2(allShifts: List[Shift]): Option[GuardStatistic] =
    getMostSleepyGuard(allShifts, { distribution =>
      if (distribution.nonEmpty)
        distribution.values.max
      else
        -1
    })
}

object RunGuards extends App {
  val events = Input
    .load("day4.txt")
    .map(Parser.parse)
    .map(_.getOrElse(throw new Exception("Failed to parse full file")))

  val shifts = Shifts(events)

  println(Guards.getMostSleepyGuard1(shifts))
  println(Guards.getMostSleepyGuard2(shifts))
}
