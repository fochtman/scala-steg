

object Wayner {

  abstract class MimicFunction

  type SubstringTable = Map[String, List[(Char, Int)]]

  /**
   * p(t, a, A): probability that a character 'a' follows a substring 't' in A
   * @param n: order of mimic function
   * @param lines: the file or txt that we are mimicking
   * @return: a map from each possible substring in 'lines' to the characters with
   *          associated weights that follow these substrings
   */
  def nthOrderFrequencies(n: Int, lines: String): SubstringTable = {
    val slices = lines.sliding(n + 1).toList
    slices groupBy(_.substring(0, n)) mapValues(xs =>
        Huffman.times(xs map(str =>
          str(n))))
  }

}
