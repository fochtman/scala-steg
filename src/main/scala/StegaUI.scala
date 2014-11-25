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
  val inputText = new TextField {
    text = "Enter message here"
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }

  var xs: List[Char] = Nil

  // takes typed input and translates it into msg
  def getMsg(mimickedCodeTree: Huffman.CodeTree): String = {
    val inputCharList = inputText.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)
    Huffman.decode(mimickedCodeTree, mimickerBits).mkString
  }

  def writeEncoded(): Unit = {
    val mimickedCodeTree = Huffman.createCodeTree(xs)(Huffman.singleton)

    val msg = getMsg(mimickedCodeTree)

    val txtStatistics = Huffman.charByWeightAndEncoding(xs)

    // exp. w/ mimicfunctions
    val sst = Wayner.nthOrderFrequencies(5, xs.mkString)
    val mf = Wayner.createMimicFunction(sst)
    val mfStr = mf.mkString("\n")

    // use mimic functions
    val bits = Wayner.createBitList(inputText.text)
    val seed = "Venus"
    //val seed = "with closed eyes"
    //val seed = "Say first"
    //val seed = "#include"
    val resu = Wayner.encode(seed, bits, mf)

    encodedText.text = (resu.split(" ")).toList.sliding(4, 4).map(_.mkString(" ")).mkString("\n")
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

      //xs = lines.mkString.trim.filter(c => c.isLetterOrDigit || c == ' ').toLowerCase.toList
      xs = lines.mkString.trim.toList
      writeEncoded()
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
      layout(new ScrollPane(inputText)) = South
    }
  }
}
