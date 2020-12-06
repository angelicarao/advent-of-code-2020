package aoc2020

import scala.annotation.tailrec

object Day4 {

  case class Passport(birthYear: Option[Int],
                      issueYear: Option[Int],
                      expirationYear: Option[Int],
                      height: Option[String],
                      hairColor: Option[String],
                      eyeColor: Option[String],
                      passportId: Option[String],
                      countryId: Option[String]) {

    private def birthYearIsValid = birthYear match {
      case Some(value) => 1920 <= value && value <= 2002
      case None => false
    }

    private def issueYearIsValid = issueYear match {
      case Some(value) => 2010 <= value && value <= 2020
      case None => false
    }

    private def expirationYearIsValid = expirationYear match {
      case Some(value) => 2020 <= value && value <= 2030
      case None => false
    }

    private def heightIsValid = height match {
      case Some(value) if value.endsWith("cm") =>
        val heightInCm = value.dropRight(2).toInt
        150 <= heightInCm && heightInCm <= 193
      case Some(value) if value.endsWith("in") =>
        val heightInCm = value.dropRight(2).toInt
        59 <= heightInCm && heightInCm <= 76
      case _ => false
    }

    private def hairColorIsValid = hairColor match {
      case Some(value) => value.matches("^#[0-9a-f]{6}$")
      case None => false
    }

    private def eyeColorIsValid = eyeColor match {
      case Some(value) =>
        value == "amb" ||
        value == "blu" ||
        value == "brn" ||
        value == "gry" ||
        value == "grn" ||
        value == "hzl" ||
        value == "oth"
      case None => false
    }

    private def passportIdIsValid = passportId match {
      case Some(value) => value.matches("^\\d{9}$")
      case None => false
    }

    def isValid: Boolean = birthYearIsValid &&
      issueYearIsValid &&
      expirationYearIsValid &&
      heightIsValid &&
      hairColorIsValid &&
      eyeColorIsValid &&
      passportIdIsValid
  }

  @tailrec
  def parseLines(lines: List[String], currentFields: List[String], passports: List[List[String]]): List[List[String]] =
    lines match {
      case head :: tail if head.isEmpty =>
        parseLines(tail, List.empty,currentFields :: passports)
      case head :: tail =>
        parseLines(tail, head :: currentFields, passports)
      case Nil =>
        currentFields :: passports
    }

  def passportFromRawLines(lines: List[String]): Passport = {
    val keyValuePairs = lines.flatMap(_.split(" ").toList)

    val birthYear = keyValuePairs.find(_.contains("byr:")).map(_.split(":").last.toInt)
    val issueYear = keyValuePairs.find(_.contains("iyr:")).map(_.split(":").last.toInt)
    val expirationYear = keyValuePairs.find(_.contains("eyr:")).map(_.split(":").last.toInt)
    val height = keyValuePairs.find(_.contains("hgt:")).map(_.split(":").last)
    val hairColor = keyValuePairs.find(_.contains("hcl:")).map(_.split(":").last)
    val eyeColor = keyValuePairs.find(_.contains("ecl:")).map(_.split(":").last)
    val passportId = keyValuePairs.find(_.contains("pid:")).map(_.split(":").last)
    val countryId = keyValuePairs.find(_.contains("cid:")).map(_.split(":").last)

    Passport(birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId, countryId)
  }

  def main(args: Array[String]): Unit = {
    val inputLines = io.Source.fromResource("day4-input.csv").getLines.toList
    val passports = parseLines(inputLines, List.empty, List.empty).map(passportFromRawLines)
    val validPassports = passports.count(_.isValid)

    println(s"The number of valid passports is $validPassports.")
  }
}
