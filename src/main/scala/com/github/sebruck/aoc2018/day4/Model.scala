package com.github.sebruck.aoc2018.day4

case class DateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int)
object DateTime {
  implicit def ordering(implicit o: Ordering[Int]): Ordering[DateTime] =
    (x: DateTime, y: DateTime) => {
      if (o.compare(x.year, y.year) != 0) {
        o.compare(x.year, y.year)
      } else if (o.compare(x.month, y.month) != 0) {
        o.compare(x.month, y.month)
      } else if (o.compare(x.day, y.day) != 0) {
        o.compare(x.day, y.day)
      } else if (o.compare(x.hour, y.hour) != 0) {
        o.compare(x.hour, y.hour)
      } else if (o.compare(x.minute, y.minute) != 0) {
        o.compare(x.minute, y.minute)
      } else 0
    }
}

sealed trait Event {
  def time: DateTime
}

sealed trait SleepAwakeEvent extends Event

case class StartShift(time: DateTime, guardId: Int) extends Event
case class FallsAsleep(time: DateTime) extends SleepAwakeEvent
case class Awakes(time: DateTime) extends SleepAwakeEvent

case class Shift(events: List[SleepAwakeEvent],
                 startedAt: DateTime,
                 guardId: Int) {

  lazy val sleepyMinutes: List[Int] =
    events
      .grouped(2)
      .flatMap {
        case List(sleep: FallsAsleep, awakes: Awakes) =>
          sleep.time.minute until awakes.time.minute
      }
      .toList
}

object Shifts {
  def apply(events: List[Event]): List[Shift] =
    events
      .sortBy(_.time)
      .foldLeft(List.empty[Shift])((acc, event) =>
        event match {
          case s: StartShift => Shift(List.empty, s.time, s.guardId) :: acc
          case e: SleepAwakeEvent =>
            acc.head.copy(events = acc.head.events :+ e) :: acc.tail
      })
      .reverse
}
