package aoc2020

import scala.annotation.tailrec

object Day5 {

  case class BoardingPass(row: Int, column: Int, seatId: Int)

  def calculateSeatId(row: Int, column: Int): Int = (row * 8) + column

  def parse(seatString: String): BoardingPass = {

    @tailrec
    def calculate(remainingChars: String, lower: Int, upper: Int, lowerChar: Char, upperChar: Char): Int = {
      remainingChars match {
        case s if s.length == 1 =>
          if (s == lowerChar.toString)
            lower
          else if (s == upperChar.toString)
            upper
          else
            throw new IllegalArgumentException
        case s =>
          val current = s.head
          val difference = (upper - lower) / 2

          if (current == lowerChar)
            calculate(remainingChars.drop(1), lower, lower + difference, lowerChar, upperChar)
          else {
            calculate(remainingChars.drop(1), upper - difference, upper, lowerChar, upperChar)
          }
      }
    }

    val rowString = seatString.take(7)
    val columnString = seatString.takeRight(3)

    val row = calculate(rowString, 0, 127, 'F', 'B')
    val column = calculate(columnString, 0, 7, 'L', 'R')
    val seatId = calculateSeatId(row, column)

    BoardingPass(row, column, seatId)
  }

  def main(args: Array[String]): Unit = {
    val seats = loadDataFromResource("day5-input.csv", parse)
    val seatIds = seats.map(_.seatId)
    val highestId = seatIds.max

    println(s"The highest seat id is $highestId.")

    val allRows = List.range(0, 128)
    val allColumns = List.range(0, 8)
    val airplaneMap = allRows.flatMap(r => allColumns.map(c => (r, c)))
    
    val presentSeats = seats.map(s => (s.row, s.column))

    val missingSeats = airplaneMap.diff(presentSeats)

    val missingSeatIds = missingSeats.map(x => calculateSeatId(x._1, x._2))

    val mySeatId = missingSeatIds.find(id => seatIds.contains(id + 1) && seatIds.contains(id - 1))

    mySeatId match {
      case Some(value) => println(s"My seat id is $value.")
      case None => println("Something went wrong.")
    }
  }
}
