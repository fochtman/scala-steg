import java.awt.FlowLayout

import scala.swing._
import scala.swing.BorderPanel.Position._
import javax.swing.ScrollPaneConstants._
import scala.swing.FileChooser._
import event._
import scala.io.Source

object StegaUI extends SimpleSwingApplication {

  val chooser = new FileChooser

  val plainText = new TextArea(20, 40) { editable = false }
  val encodedText = new TextArea(20, 40) { editable = false }

  val inputSeed = new TextField {
    columns = 75
    text = "select substring from loaded text"
  }

  val inputMsg = new TextField {
    columns = 75
    text = "type msg here, then hit enter"
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }

  val msgPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label("MSG: "), inputMsg)
  val seedPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label("SEED: "), inputSeed)
  val inputPanel = new BorderPanel {
    layout(seedPanel) = North
    layout(msgPanel) = South
  }

  //var xs: List[Char] = Nil
  var xs: List[String] = Nil

  // takes typed input and translates it into msg
  def getMsg(mimickedCodeTree: Huffman.CodeTree): String = {
    val inputCharList = inputMsg.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)
    Huffman.decode(mimickedCodeTree, mimickerBits).mkString
  }

  def writeEncoded(): Unit = {
    /*
    val mimickedCodeTree = Huffman.createCodeTree(xs)(Huffman.singleton)
    val msg = getMsg(mimickedCodeTree)
    val txtStatistics = Huffman.charByWeightAndEncoding(xs)
    */

    // exp. w/ mimicfunctions
    val sst = Wayner.nthOrderFrequencies(10, xs.mkString("\n"))
    val mf = Wayner.createMimicFunction(sst)

    // use mimic functions
    val bits = Wayner.createBitList(inputMsg.text)
    val seed = inputSeed.text
    val resu = Wayner.encode(seed, bits, mf)

    encodedText.text = resu
    //encodedText.text = msg + "\n\n" + txtStatistics + "\n\n" + mfStr
    encodedText.caret.position = 0
  }

  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      plainText.caret.position = 0

      //xs = lines.mkString.trim.toList
      xs = lines
    }
  }

  def fileMenu = new Menu("File") {
    contents += new MenuItem(Action("Open") {open()})
    contents += new MenuItem(Action("Exit") {sys.exit(0)})
  }

  def top = new MainFrame {
    title = "Scala Steganography"

    menuBar = new MenuBar {
      contents += fileMenu
    }

    contents = new BorderPanel {
      layout(new ScrollPane(plainText)) = East
      layout(new ScrollPane(encodedText)) = West
      //layout(new ScrollPane(inputText)) = South
      layout(inputPanel) = South
    }
  }
}
