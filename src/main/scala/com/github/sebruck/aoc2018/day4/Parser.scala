package com.github.sebruck.aoc2018.day4

import scala.util.Try

object Parser {
  def parse(line: String): Option[Event] = {
    val LinePattern =
      "\\[(\\d+)-(\\d+)-(\\d+)\\s(\\d+):(\\d+)\\][a-zA-Z\\s]*[#]?(\\d+)?.*".r

    line match {
      case LinePattern(year, month, day, hour, second, guardId) =>
        Try {
          val dt =
            DateTime(year.toInt,
                     month.toInt,
                     day.toInt,
                     hour.toInt,
                     second.toInt)

          val event = if (line.contains("begins shift")) {
            require(guardId != null)
            StartShift(dt, guardId.toInt)
          } else if (line.contains("falls asleep")) {
            FallsAsleep(dt)
          } else if (line.contains("wakes up")) {
            Awakes(dt)
          } else {
            throw new Exception(s"Can not parse line $line")
          }

          event
        }.toOption
      case _ => None
    }
  }
}
