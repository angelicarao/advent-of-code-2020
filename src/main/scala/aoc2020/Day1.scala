package aoc2020

object Day1 {

  private val lookingFor = 2020

  def main(args: Array[String]): Unit = {
    val input = loadDataFromResource("day1-input.csv", _.toInt)

    val inputCopy = input
    val inputCopy2 = input

    for (i <- input) {
      for (j <- inputCopy) {
        for (z <- inputCopy2) {
          if (i + j + z == lookingFor) {
            println(s"Numbers are $i, $j and $z")

            val result = i * j * z
            println(s"If you multiply them the result is $result")
          }
        }
      }
    }

  }
}
