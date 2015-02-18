import Huffman._
import scala.annotation.tailrec
import scala.util.Random

/**
 * A functional implementation of a mimic function as defined by
 * Peter Wayner in his seminal paper 'Mimic Functions'.
 *
 * Mimic Functions overview:
 * 1. p(t, A): probability of substring t occurring in A
 * 2. p(t, f(A)): f is a mimic function, s.t. A is recoded to approximate p(t, B)
 *      for all strings t of length less than some n.
 *      Imagine B as Shakespeare and f(A) as some text that is similar to Shakespeare
 *      from a statistical standpoint
 * 3. p(t, a, A): probability that a character 'a' follows a substring 't' in A
 * 4. p(nil, a, A): independent probability of 'a' occurring in A
 * 5. p(t, nil, A): independent probability of 't' occurring in A
 * 6. |p(t, nil, A) - p(t, nil B)| < epsilon: files A & B are statistically equivalent
 *      within epsilon for all substrings 't' of length less than n.
 *
 * Have the following functionality:
 * 1. f(A, B): see Huffman.encode. Compress B based on statistical profile of A
 * 2. g(A, B): see Huffman.decode. Expand B based on statistical profile of A
 *             this function is the inverse of f(A, B).
 *             this function takes the bit string B and maps variable length slices
 *             of B to fixed length slices of the original file which the Huffman.CodeTree
 *             is derived from.
 * 3. g(A, f(B, B)): see TODO. First order mimic function.
 *                   functional composition g of f.
 *
 * Other Mimic Function Terminology:
 * 1. first order equivalence: the case when n = 1
 * 2. digrams, trigarms: the case when n = 2 or n = 3 respectively and their
 *      associated probability
 */

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

  def decodeBits(xs: List[Bit]): String = {
    val strVecs = xs.sliding(7,7).toVector.map(_.mkString)
    strVecs.foreach(println)
    val res = strVecs map(s => Integer.parseInt(s, 2).toChar)
    res.mkString
  }

  case class Dim(rows: Int, cols: Int)
  case class BitVector(v: Vector[Bit], asInt: Int, asStr: String)
  case class BitMatrix(vectors: Vector[BitVector], dim: Dim)

  def makeMatrices(bits: Vector[Bit], permutationFlag: Boolean = false): Vector[BitMatrix] = {

    val bitVec: Vector[Vector[Bit]] =
      if (!permutationFlag) Vector(bits)
      else {
        val v = bits.sliding(8, 8).toVector.permutations.toVector
        v map(_.flatten)
      }

    val len = bits.length
    val factors: Vector[Dim] =
      for {
        i <- (2 to len - 1).toVector
        if len % i == 0
      } yield Dim(len / i, i)

    for {
      vector <- bitVec
      Dim(rows, cols) <- factors
    } yield {
      val vectors = vector.sliding(cols, cols).toVector
      val matrix =
        vectors map(vec => {
          val s = vec.mkString
          val i = Integer.parseInt(s, 2)
          BitVector(vec, i, s)
        })
      BitMatrix(matrix, Dim(rows, cols))
    }
  }

  def f(a: BitVector, cols: Int): Int = {
    val ones = Integer.parseInt("1"*cols, 2)
    val i = a.asInt
    ((i << 1) & ones) ^ i ^ (i >> 1)
  }

  def makeVectorOfZeros(matrix: BitMatrix): BitVector = {
    val len = matrix.dim.cols
    val zeros = Vector.fill(len)(0)
    BitVector(zeros, 0, "0" * len)
  }

  def makeBitString(i: Int, len: Int): String = {
    val asStr = i.toBinaryString
    if (asStr.length < len)
      asStr.reverse.padTo(len, "0").reverse.mkString
    else
      asStr
  }

  def makeNextRow(r0: BitVector, r1: BitVector, cols: Int): BitVector = {
    val asInt = f(r1, cols) ^ r0.asInt
    val asStr = makeBitString(asInt, cols)
    val asVec = (asStr map { case '1' => 1; case '0' => 0}).toVector
    BitVector(asVec, asInt, asStr)
  }

  /**
  * Due to the huge search space should do this recursively and exit
  * recursion and soon as the value returned from 'makeNextRow'
  * isn't a row in the original
  * As of right now we continue (futilely) to iterate through the rows
  * even if we know that this matrix cannot produce a parity pattern
  */
  def makeParityPatterns(matrices: Vector[BitMatrix]): Vector[BitMatrix] = {
    val mutMap = scala.collection.mutable.Map[Int, BitVector]()
    for (matrix <- matrices) yield {
      val zeros = makeVectorOfZeros(matrix)
      val initialRow = matrix.vectors(0)
      mutMap += 0 -> zeros
      mutMap += 1 -> initialRow
      val Dim(rowCount, colCount) = matrix.dim
      val intermediateVectors: Vector[BitVector] =
        for (j <- (1 to rowCount + 1).toVector) yield {
          val lastRow = mutMap(j - 1)
          val thisRow = mutMap(j)
          val nextRow = makeNextRow(lastRow, thisRow, colCount)
          mutMap += (j + 1) -> nextRow
          thisRow
        }
      mutMap.clear()
      BitMatrix(intermediateVectors, matrix.dim)
    }
  }

  def checkMatrixDimensions(matrix: BitMatrix): Boolean = {
    val rowValues = (matrix.vectors map(vec => vec.v.length)).distinct
    val last = matrix.vectors.last.v
    if (rowValues.length == 1 && last.forall(_ == 0)) true
    else false
  }

  def check(derived: Vector[BitMatrix], original: Vector[BitMatrix]): Vector[BitMatrix] = {
    for {
      (matrix, index) <- derived.zipWithIndex
      if checkMatrixDimensions(matrix)
      dims = matrix.dim
      vecs = matrix.vectors.take(dims.rows)
    } yield BitMatrix(vecs, dims)
  }

  /**
   * This function is playing around with some ideas from Knuth's TAOCP VOL. 7
   * In particular the question concerning parity patterns.
   *
   * TODO: move this out of Wayner.scala (as it has little to do with it)
   */
  def analyzeBitList(bits: List[Bit]): Unit = {

    val matrices = makeMatrices(bits.toVector)

    val performPermutations = true
    val permMatrices = makeMatrices(bits.toVector, performPermutations)

    val normal = makeParityPatterns(matrices)
    val others = makeParityPatterns(permMatrices)

    val results0 = check(normal, matrices)
    val results1 = check(others, permMatrices)

    val res0 =
      for (r <- results0) yield {
        val v = r.vectors
        val bigVec = v flatMap(_.v)
        decodeBits(bigVec.toList)
      }

    val res1 =
      for (r <- results1) yield {
        val v = r.vectors
        val bigVec = v flatMap(_.v)
        decodeBits(bigVec.toList)
      }

    println("RESULTS0 => ")
    res0.foreach(println)
    println("RESULTS1 => ")
    res1.foreach(println)

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

  // currently probably inefficient due to repeated concatenation of strings in recursive call
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
}
