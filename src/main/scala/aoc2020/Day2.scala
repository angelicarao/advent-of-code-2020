package aoc2020

object Day2 {

  case class PasswordPolicy(lowerBound: Int, upperBound: Int, letter: Char, password: String)

  def parse(passwordInput: String): PasswordPolicy = {
    val policyAndPass = passwordInput.split(':')
    val boundsAndLetter = policyAndPass(0).split(' ')
    val bounds = boundsAndLetter(0).split('-')

    val letter = boundsAndLetter(1).charAt(0)
    val password = policyAndPass(1).trim

    PasswordPolicy(bounds(0).toInt, bounds(1).toInt, letter, password)
  }

  def isValidPartOne(passwordPolicy: PasswordPolicy): Boolean = {
    val occursTimes = passwordPolicy.password.count(_ == passwordPolicy.letter)

    if (passwordPolicy.lowerBound <= occursTimes && occursTimes <= passwordPolicy.upperBound)
      true
    else
      false
  }

  def isValidPartTwo(passwordPolicy: PasswordPolicy): Boolean = {
    val indexOne = passwordPolicy.lowerBound - 1
    val indexTwo = passwordPolicy.upperBound - 1

    if (passwordPolicy.password.charAt(indexOne) == passwordPolicy.letter && passwordPolicy.password.charAt(indexTwo) != passwordPolicy.letter)
      true
    else if (passwordPolicy.password.charAt(indexOne) != passwordPolicy.letter && passwordPolicy.password.charAt(indexTwo) == passwordPolicy.letter)
      true
    else
      false
  }

  def main(args: Array[String]): Unit = {
    val input = loadDataFromResource("day2-input.csv", parse)

    val result1 = input.map(isValidPartOne).count(_ == true)
    val result2 = input.map(isValidPartTwo).count(_ == true)

    println(s"The number of valid passwords for Part One is $result1")
    println(s"The number of valid passwords for Part Two is $result2")
  }
}
