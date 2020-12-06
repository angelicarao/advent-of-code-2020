package object aoc2020 {

  /**
   * Loads data from a file in resources and maps each line over the provided parsing function.
   *
   * @param resourcePath The path to the resource to load.
   * @param parsingFn Function which converts a string line to A.
   * @tparam A Type parameter for the data type which is loaded.
   * @return A collection of each line of the resource, parsed over the parser function.
   */
  def loadDataFromResource[A](resourcePath: String, parsingFn: String => A): List[A] = {
    val file = io.Source.fromResource(resourcePath)
    file.getLines.map(parsingFn).toList
  }

}
