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

  def decodeBits(xs: List[Bit]): String = {
    val strVecs = xs.sliding(7,7).toVector.map(_.mkString)
    strVecs.foreach(println)
    val res = strVecs map(s => Integer.parseInt(s, 2).toChar)
    res.mkString
  }

  case class Dim(rows: Int, cols: Int)
  case class BitVector(v: Vector[Bit], asInt: Int, asStr: String)
  case class BitMatrix(vectors: Vector[BitVector], dim: Dim)

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

  def analyzeBitList(bits: List[Bit]): Unit = {


    val len = bits.length
    val factors: Vector[Dim] =
      for {
        i <- (2 to len - 1).toVector
        if len % i == 0
      } yield Dim(len / i, i)

    val matrices: Vector[BitMatrix] =
      for {
        Dim(rows, cols) <- factors
      // to find all permutations add arg.permutations.toVector
      } yield {
        val vectors = bits.toVector.sliding(cols, cols).toVector
        val matrix =
          vectors map(vec => {
            val s = vec.mkString
            val i = Integer.parseInt(s, 2)
            BitVector(vec, i, s)
          })
        BitMatrix(matrix, Dim(rows, cols))
      }


    val mutMap = scala.collection.mutable.Map[Int, BitVector]()

    val intermediateMatrices: Vector[BitMatrix] =
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

    def checkMatrixDimensions(matrix: BitMatrix): Boolean = {
      val rowValues = (matrix.vectors map(vec => vec.v.length)).distinct
      val last = matrix.vectors.last.v
      if (rowValues.length == 1 && last.forall(_ == 0)) true
      else false
    }

    val results =
      for {
        (matrix,index) <- intermediateMatrices.zipWithIndex
        if checkMatrixDimensions(matrix)
        dims = matrix.dim
        vecs = matrix.vectors.take(dims.rows)
        //if vecs == matrices(index).vectors
      } yield BitMatrix(vecs, dims)

    intermediateMatrices.foreach(println)
    println("RESULTS => ")
    results.foreach(r => println(r, r.vectors.length))

    for (r <- intermediateMatrices) { //results) {
      val v = r.vectors
      val bigVec = v flatMap(_.v)
      decodeBits(bigVec.toList)
    }

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
