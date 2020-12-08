package aoc2020

object Day6 {

  case class Answer(questionCharacter: Char)
  case class Person(answers: List[Answer])

  def parseGroup(group: String): List[Person] =
    group
      .split("\r\n")
      .map(x => Person(x.map(Answer).toList))
      .toList


  def main(args: Array[String]): Unit = {
    val rawInputGroups = io.Source.fromResource("day6-input.csv").mkString.split("\r\n\r\n")
    val parsedGroups = rawInputGroups.map(parseGroup)
    val allDistinctAnswersPerGroup = parsedGroups.map(group => group.flatMap(_.answers).distinct.size)
    
    println(s"The number of distinct answers is ${allDistinctAnswersPerGroup.sum}.")

    val distinctAllInGroup = parsedGroups
      .map { group =>
          group.map(_.answers).reduce((x, y) => x.intersect(y)).size
      }

    println(s"The number of distinct answers to which everyone answered 'yes' is ${distinctAllInGroup.sum}")
  }
}
