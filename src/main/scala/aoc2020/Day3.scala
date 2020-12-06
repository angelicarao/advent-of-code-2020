package aoc2020

import scala.annotation.tailrec

object Day3 {

  sealed trait GeoObject

  case class Square() extends GeoObject

  case class Tree() extends GeoObject

  val squareRepresentation = '.'
  val treeRepresentation = '#'

  def parseRow(row: String): List[GeoObject] =
    row.toCharArray.map(c => if (c == treeRepresentation) Tree() else Square()).toList

  @tailrec
  def traverse(slope: List[List[GeoObject]], currentPosition: Int, moveRight: Int, moveDown: Int, encountered: List[GeoObject]): List[GeoObject] =
    slope match {
      case List() => encountered
      case onlyElement :: Nil => onlyElement(currentPosition) :: encountered
      case s =>
        val encounteredObj = s.head(currentPosition)

        val tail = s.drop(moveDown)
        val line = tail.head

        val newPosition = (currentPosition + moveRight) % line.size

        traverse(tail, newPosition, moveRight, moveDown, encounteredObj :: encountered)
    }

  def main(args: Array[String]): Unit = {
    val slope = loadDataFromResource("day3-input.csv", parseRow)

    val encounteredTrees = traverse(slope, 0, 3, 1, List.empty).map {
      case Tree() => true
      case Square() => false
    }.count(_ == true)

    val methods = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

    val encounteredTreesTwo =
      methods.map(x => traverse(slope, 0, x._1, x._2, List.empty).map {
        case Tree() => true
        case Square() => false
      }.count(_ == true)).map(_.toLong).product

    println(s"I have encountered $encounteredTrees trees.")
    println(s"The product of encountered trees is $encounteredTreesTwo.")

  }
}
