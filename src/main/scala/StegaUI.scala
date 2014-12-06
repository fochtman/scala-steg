import javax.swing.border.EmptyBorder

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import scala.io.Source
import scala.swing.Orientation._
object StegaUI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Scala Steganography"

    menuBar = new MenuBar {
      contents += fileMenu
    }

    contents = new BorderPanel {
      layout(subPanel) = Center
      layout(inputPanel) = South
    }

    font = consolasFont
  }

  val chooser = new FileChooser

  val consolasFont = new Font("Consolas", java.awt.Font.PLAIN, 12)

  val plainText = new TextArea(20, 40) {
    editable = false
    wordWrap = true
    font = consolasFont
  }
  val encodedText = new TextArea(20, 40) {
    peer.setAlignmentX(0)
    editable = false
    wordWrap = true
    font = consolasFont
  }

  val subPanel = new SplitPane(Vertical, new ScrollPane(encodedText), new ScrollPane(plainText)) {
    border = new EmptyBorder(8, 8, 8, 8)
  }

  val randomSeed = new Button {
    text = "RND SEED"
    font = consolasFont
    enabled = true
  }
  val inputSeed = new TextField {
    columns = 74
    font = consolasFont
    text = "select substring from loaded text"
  }

  val inputMsg = new TextField {
    columns = 75
    font = consolasFont
    text = "type msg here, then hit enter"
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }

  val seedLength = 6
  val arrow =  new String(Character.toChars(8594))
  val msgPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label{text = "MSG "+arrow; font = consolasFont}, inputMsg)
  val seedPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label{text = "SEED "+arrow; font = consolasFont}, inputSeed, randomSeed)

  val inputPanel = new BorderPanel {
    layout(seedPanel) = North
    layout(msgPanel) = South
    listenTo(randomSeed)
    reactions += {
      case ButtonClicked(`randomSeed`) =>
        val x = scala.util.Random.nextInt(plainText.text.length)
        val subStr = plainText.text.substring(x, x + seedLength)
        inputSeed.text = subStr
    }
  }

  var xs: List[String] = Nil

  // takes typed input and translates it into msg
  def getMsg(mimickedCodeTree: Huffman.CodeTree): String = {
    val inputCharList = inputMsg.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)
    Huffman.decode(mimickedCodeTree, mimickerBits).mkString
  }

  def writeEncoded(): Unit = {
    //val txtStatistics = Huffman.charByWeightAndEncoding(xs)

    val sst = Wayner.nthOrderFrequencies(seedLength, xs.mkString("\n"))
    val mf = Wayner.createMimicFunction(sst)

    val bits = Wayner.createBitList(inputMsg.text)
    val seed = inputSeed.text
    val resu = Wayner.encode(seed, bits, mf)

    encodedText.text = resu
    encodedText.caret.position = 0
  }

  /*
  for reading files look into:
  import java.nio.file.{Files, FileSystems}
  import java.nio.charset.StandCharsets.UTF_8
  val path = FileSystems.getDefault().getPath(".", fileName)
  val lines = Files.readAllLines(path, UTF_8)
  val final = lines.toArray map(_.toString)
   */
  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      plainText.caret.position = 0

      //xs = (lines map(_.reverse)) //::: lines
      xs = lines
    }
  }

  def fileMenu = new Menu("File") {
    font = consolasFont
    contents += new MenuItem(Action("Open") {open()})
    contents += new MenuItem(Action("Exit") {sys.exit(0)})
  }

}
