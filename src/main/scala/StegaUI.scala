import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import scala.io.Source
import scala.swing.Orientation._
import java.awt.Color._
object StegaUI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "scala-steg"

    menuBar = new MenuBar with compColor {
      background = DARK_GRAY
      contents += fileMenu
    }
    contents = new BorderPanel with compColor {
      layout(subPanel) = Center
      layout(inputPanel) = South
    }
    font = consolasFont
    background = ivoryBlack
    foreground = yellowOchre
  }

  val chooser = new FileChooser
  val consolasFont = new Font("Consolas", java.awt.Font.PLAIN, 16)
  val ivoryBlack = new Color(40, 36, 34)
  val yellowOchre = new Color(245, 197, 44)
  val cadmiumRedMedium = new Color(196, 1, 45)

  trait compColor extends Component {
    font = consolasFont
    background = ivoryBlack
    foreground = yellowOchre
  }

  val plainText = new TextArea(20, 40) with compColor {
    editable = false
    wordWrap = true
  }

  val encodedText = new TextArea(20, 40) with compColor {
    editable = false
    wordWrap = true
  }

  val subPanel = new SplitPane(Vertical, new ScrollPane(encodedText), new ScrollPane(plainText)) with compColor {
    dividerSize = 3
    peer.setDividerLocation(0.5f)
  }

  // a constant used in the seed and msg panels
  val arrow =  new String(Character.toChars(8594))

  // seedPanel parameters, begin
  val seedPanelLabel = new Label with compColor { text = s"SEED $arrow" }
  val inputSeed = new TextField with compColor {
    text = "select substring from loaded text"
    columns = 74
  }
  val randomSeed = new Button with compColor {
    text = "RND SEED"
    enabled = true
  }
  val seedLength = new ComboBox(1 to 20) { peer.setSelectedIndex(9) }
  val seedPanel = new FlowPanel(FlowPanel.Alignment.Center)(seedPanelLabel, inputSeed, randomSeed, seedLength) with compColor
  // seedPanel parameters, end

  // msgPanel parameters, begin
  val msgPanelLabel = new Label with compColor { text = s"MSG $arrow" }
  val inputMsg = new TextField with compColor {
    text = "type msg here, then hit enter"
    columns = 75
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }
  val msgPanel = new FlowPanel(FlowPanel.Alignment.Center)(msgPanelLabel, inputMsg) with compColor
  // msgPanel parameters, end

  val inputPanel = new BorderPanel with compColor {
    layout(seedPanel) = North
    layout(msgPanel) = South
    listenTo(randomSeed)
    reactions += {
      case ButtonClicked(`randomSeed`) =>
        val x = scala.util.Random.nextInt(plainText.text.length)
        val subStr = plainText.text.substring(x, x + seedLength.selection.item)
        inputSeed.text = subStr
    }
  }

  var xs: List[String] = Nil

  /**
   * Takes user input and encodes it based on the statistical profile
   * of the loaded text. In this case we derive the profile via Huffman
   * compression.
   *
   * Currently this method isn't used, but serves as an example of what
   * Wayner's mimic functions do (leaving out some details) for every
   * n-length substring of the loaded text.
   */
  def getMsg(mimickedCodeTree: Huffman.CodeTree): String = {
    val inputCharList = inputMsg.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)
    Huffman.decode(mimickedCodeTree, mimickerBits).mkString
  }

  def checkBits(): Unit = {
    val bits = (inputMsg.text  map{ case '1' => 1; case '0' => 0 }).toList
    Wayner.analyzeBitList(bits)
  }

  def writeEncoded(): Unit = {
    val sst = Wayner.nthOrderFrequencies(seedLength.selection.item, xs.mkString("\n"))
    val mf: Wayner.MimicFunction = Wayner.createMimicFunction(sst)
    val bits = Wayner.createBitList(inputMsg.text)

    println(Wayner.decodeBits(bits))
    //Wayner.analyzeBitList(bits)

    val seed = inputSeed.text
    val resu = Wayner.encode(seed, bits, mf)

    val xss = for {
      subStr: String <- resu.split("\n").toList
    } yield {
      if (subStr.length > 100) {
        val subXs = subStr.toVector
        var splitPoint = subXs.length / 2
        var c = subXs(splitPoint)
        while (c != ' ') {
          splitPoint += 1
          c = subXs(splitPoint)
        }
        val (left, right) = subXs.splitAt(splitPoint)
        left.mkString + "\n" + right.mkString
      } else
        subStr
    }

    val sizeResult = resu.toList.length * 16
    val sizeSeed = inputSeed.text.length * 16
    val sizeMsg = inputMsg.text.length * 16
    val str = s"msg: $sizeMsg\nseed: $sizeSeed\nresult: $sizeResult\n"
    encodedText.text = str + xss.mkString("\n")
    encodedText.caret.position = 0
  }

  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      plainText.caret.position = 0

      xs = lines
    }
  }

  def fileMenu = new Menu("File") {
    contents += new MenuItem(Action("Open") {open()})
    contents += new MenuItem(Action("Exit") {sys.exit(0)})
  }

}
