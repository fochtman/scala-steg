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

  def analyzeBitList(bits: List[Bit]): Unit = {

    val bitVector = bits.toVector
    val len = bits.length
    val factors: Vector[(Int, Int)] =
      for {
        i <- (2 to len - 1).toVector
        if len % i == 0
      } yield (i, len / i)

    //type BitMatrix = Vector[Vector[Bit]]
    case class BitVector(v: Vector[Bit], asInt: Int, asStr: String, len: Int)
    type BitMatrix = Vector[BitVector]
    val matrices: Vector[BitMatrix] =
      for {
        (rows, cols) <- factors
      // to find all permutations add arg.permutations.toVector
      } yield {
        val matrix = bitVector.sliding(rows, rows).toVector
        matrix map(vec => {
          val s = vec.mkString
          val i = Integer.parseInt(s, 2)
          BitVector(vec, i, s, rows)
        })
      }

    for (m <- matrices; v <- m) println(v.asStr)

    val zeroVec = {
      val aVec = matrices(0)(0)
      val len = aVec.len
      val zeros = Vector.fill(len)(0)
      BitVector(zeros, 0, "0"*len, len)
    }

    def f(a: BitVector): Int = {
      val ones = ("1"*a.len).toInt
      val i = a.asInt
      ((i << 1) & ones) ^ i ^ (i >> 1)
    }

    def makeBitString(i: Int, len: Int): String = {
      val asStr = i.toBinaryString
      if (asStr.length != len)
        asStr.reverse.padTo(len, "0").reverse.mkString
      else
        asStr
    }

    val initVec = matrices(0)(0)
    val mutMap = scala.collection.mutable.Map[Int, BitVector](0 -> zeroVec, 1 -> initVec)

    val compMat =
      for (j <- (1 to matrices(0).length).toVector) yield {
        val asInt = f(mutMap(j)) ^ mutMap(j - 1).asInt
        val asStr = makeBitString(asInt, mutMap(j).len)
        val asVec = (asStr map { case '1' => 1; case '0' => 0}).toVector
        val nextRow = BitVector(asVec, asInt, asStr, asStr.length)
        mutMap += (j+1) -> nextRow
        mutMap(j)
      }



    //println("\ncompMat: ")
    //compMat.foreach(println)
    println(matrices(0)==compMat)


    println(bits)
    println(factors)
    /*
    def func(xs: List[String]): Boolean = {
      val v = c(xs:_*)
      val l = xs.head.length
      val m = Map[Int, Int](-1 -> 0)
      val r = (v zipWithIndex).map{ case(i, j) => {m(j) = f(i, l) ^ m(j-1); f(i, l) ^ m(j-1)} }
      */
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

  // currently probably inefficient due to repeated concatentation of strings in recursive call
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
