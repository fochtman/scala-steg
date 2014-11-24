import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding from coursera.org
 * Class taught by: Martin Odersky
 * Solution: Tyler Fochtman
 */

/**
 * Mimic Functions:
 * Need the following functionalization:
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
 */


/**
 * Mimic Function Terminology:
 * 1. first order equivalence: the case when n = 1
 * 2. digrams, trigarms: the case when n = 2 or n = 3 respectively and their
 *      associated probability
 */

/**
 * Changes to Huffman:
 * 1. Need to make it so that I can break files into variable length blocks,
 *      not just break it into characters
 */

object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

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

  /*
  //TODO FINISH THIS THOUGHT
  def processChars(chars: List[Char]): List[String] =
    chars.sliding(variableOne, variableTwo).toList map(_.mkString)
  */

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

  /** Decoding */
  type Bit = Int

  /**
   * decode: expands 'bits' based on statistical profile embedded in 'tree'
   *         this is the inverse of Huffman.encode, or Wayner.f(_, _).
   *         Wayner represents Huffman.decode as g(A, B)
   * @param tree: statistical profile of some file A
   * @param bits: potentially random list of bits, could be the encoding of some file B,
   *              from Wayner this would look like f(B, B). composing the functions
   * @return: for each variable length slice of 'bits', produces a single Char. 
   */

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


  /** Encoding */

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

  /** Encoding with EncodeTable (memoize the tree) */
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


  /**
   * UTILITIES
   */

  def charByWeightAndEncoding(xs: List[Char]): String = {
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
  /**
   * Current issue with using different trees texts
   * is that a list of bits which terminates at a leaf in one tree
   * may terminate at a fork in another
   */
  /*
  def quickEncode(tree: CodeTree)(text: List[Char]): List[List[Bit]] = {
    val codeMap = Map(treeToEncodeTable(tree):_*)
    text map (c => codeMap(c))
  }

  type DecodeTable = List[(List[Bit], Char)]

  def treeToDecodeTable(tree: CodeTree): DecodeTable =
    treeToEncodeTable(tree) map {case (char, bitList) => (bitList, char)}

  //def createCodeTree(chars: List[Char])(targetOrder: List[CodeTree] => Boolean): CodeTree = {
  //def quickDecode(tree: CodeTree, chars: List[Char])(bits: List[Bit]): List[Char] = {
  def quickDecode(encTab: EncodeTable, chars: List[Char])(bits: List[Bit]): List[Char] = {
    val codeMap =
    val codeMap = Map(treeToDecodeTable(tree):_*)
    bits map (b => codeMap(b))
  }
  */
  /** Test Case */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  // huffmanestcool
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  def decodedSecret: List[Char] = decode(frenchCode, secret)
}
