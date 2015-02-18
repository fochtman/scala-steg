import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding from coursera.org
 * Class taught by: Martin Odersky
 * Solution: Tyler Fochtman
 */


object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, c, weight) => weight
    case Leaf(c, weight) => weight
    case _ => throw new NoSuchElementException
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, chars, w) => chars
    case Leaf(char, w) => char :: Nil
    case _ => throw new NoSuchElementException
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =
    (chars groupBy(c => c) map {case (c, xs) => (c, xs.length)}).toList

  def makeOrderedLeafList(frequencies: List[(Char, Int)]): List[Leaf] =
    frequencies.sortBy(_._2) map {case (l, r) => Leaf(l, r)}

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => Nil
    case singleton :: Nil => singleton :: Nil
    case first :: second :: Nil => List(makeCodeTree(first, second))
    case first :: second :: tail =>
      val newFork = makeCodeTree(first, second)
      val (lessWeight, greaterWeight) = tail.partition(t => weight(t) < weight(newFork))
      lessWeight ::: newFork :: Nil ::: greaterWeight
  }

  @tailrec
  def until(targetOrder: List[CodeTree] => Boolean, treeOp: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    if (targetOrder(trees))
      trees
    else
      until(targetOrder, combine)(combine(trees))

  def createCodeTree(chars: List[Char])(targetOrder: List[CodeTree] => Boolean): CodeTree = {
    val orderedLeaves = makeOrderedLeafList(times(chars))
    until(targetOrder, combine)(orderedLeaves).head
  }

  /** Decoding methods */
  /**
   * decode: expands 'bits' based on statistical profile embedded in 'tree'
   *         this is the inverse of Huffman.encode, or Wayner.f(_, _).
   *         Wayner represents Huffman.decode as g(A, B)
   * @param tree: statistical profile of some file A
   * @param bits: potentially random list of bits, could be the encoding of some file B,
   *              from Wayner this would look like f(B, B). composing the functions
   * @return: for each variable length slice of 'bits', produces a single Char. 
   */

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decoder(subTree: CodeTree, subBits: List[Bit]): List[Char] = (subTree, subBits) match {
      case (Fork(leftTree, rightTree, chars, _), head :: tail) =>
        if (head == 0)
          decoder(leftTree, tail)
        else
          decoder(rightTree, tail)
      case (Fork(leftTree, rightTree, chars, _), Nil) =>
        chars
      case (Leaf(char, _), head :: tail) =>
        char :: decoder(tree, subBits)
      case (Leaf(char, _), Nil) =>
        char :: Nil
    }
    decoder(tree, bits)
  }


  /** Encoding methods */
  /**
   * encode: compresses 'text' based on the statistical profile embedded in 'tree'
   * @param tree: statistical profile of some file A
   * @param text: arbitrary list of Chars. Can be file A, or some other file B
   * @return: optimal compression of 'text' based on 'tree'
   *
   * In Mimic Functions this function is referred to as f(A, B)
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encoder(t: CodeTree, txt: List[Char], acc: List[Bit]): List[Bit] =
      txt match {
        case head :: tail =>
          t match {
            case Fork(left, right, _, _) =>
              encoder(left, txt, 0 :: acc) ::: encoder(right, txt, 1 :: acc)
            case Leaf(char, _) =>
              if (head == char) encoder(tree, tail, acc)
              else Nil
          }
        case Nil => acc
      }
    encoder(tree, text, Nil).reverse
  }

  type EncodeTable = List[(Char, List[Bit])]

  @tailrec
  def codeBits(table: EncodeTable)(char: Char): List[Bit] = table match {
    case (c, bits) :: tail =>
      if (c == char) bits
      else codeBits(tail)(char)
    case Nil => Nil
  }

  // aka convert
  def treeToEncodeTable(tree: CodeTree): EncodeTable = {
    def helper(t: CodeTree, acc: List[Bit]): EncodeTable = t match {
      case Fork(l, r, chars, _) =>
        helper(l, 0 :: acc) ::: helper(r, 1 :: acc)
      case Leaf(c, _) =>
        (c, acc.reverse) :: Nil
    }
    helper(tree, Nil)
  }

  def mergeEncodeTables(a: EncodeTable, b: EncodeTable): EncodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeMap = Map(treeToEncodeTable(tree):_*)
    text flatMap (c => codeMap(c))
  }

  def printStatisticalProfile(xs: List[Char]): String = {
    val codeTree = Huffman.createCodeTree(xs)(Huffman.singleton)
    val length = xs.length
    val encodingByChar = Huffman.codeBits(Huffman.treeToEncodeTable(codeTree))_

    "Char\tWeight\tProb.*\tEncoding\n" +
    (for {
      (char, weight) <- Huffman.times(xs).sortBy(_._2).reverse
      wl = s"$weight/$length"
      bs = encodingByChar(char).mkString
      p = s"1/${1 << bs.length}"
    } yield s"$char: \t $wl \t $p \t $bs\n").mkString +
    "\n\n" +
    "* => Probability of occurrence given random bit list."
  }
}
