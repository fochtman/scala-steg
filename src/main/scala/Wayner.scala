import Huffman._
import scala.annotation.tailrec
import scala.util.Random

object Wayner {

  type Substring = String
  type MimicFunction = Map[Substring, Huffman.CodeTree]
  type SubstringTable = Map[Substring, List[(Char, Int)]]

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

  def createMimicFunction(table: SubstringTable): MimicFunction = {
    (for {
      (subStr, xs) <- table
      orderedLeaves = makeOrderedLeafList(xs)
      codeTree = Huffman.until(singleton, combine)(orderedLeaves).head
    } yield (subStr, codeTree)).toMap
  }

  def createBitList(input: String): List[Bit] = {
    val bitStr = input flatMap (c => c.toInt.toBinaryString)
    (bitStr  map{ case '1' => 1; case '0' => 0 }).toList
  }

  def shallowEncoder(tree: CodeTree, bits: List[Bit]): (String, List[Bit]) = (tree, bits) match {
      case (Fork(leftTree, rightTree, chars, _), head :: tail) =>
        if (head == 0)
          shallowEncoder(leftTree, tail)
        else
          shallowEncoder(rightTree, tail)

      // padding
      case (Fork(leftTree, rightTree, chars, _), Nil) =>
        val randomBit = Random.nextInt(2)
        if (randomBit == 0)
          shallowEncoder(leftTree, Nil)
        else
          shallowEncoder(rightTree, Nil)

      // once we reach a leaf we return
      case (Leaf(char, w), xs) =>
        (char.toString, xs)
  }

  @tailrec
  def encode(seed: String, bits: List[Bit], mimFunc: MimicFunction): String = {
    if (bits.isEmpty)
      seed
    else {
      val keyLength = mimFunc.keys.head.length
      val keySeed = seed.drop(seed.length - keyLength)
      val codeTree = mimFunc(keySeed)
      val (result, remainingBits) = shallowEncoder(codeTree, bits)
      encode(seed + result, remainingBits, mimFunc)
    }
  }
  // import scala.util.Random
  // val binaryStream = Stream(Random.nextInt(2))

}
