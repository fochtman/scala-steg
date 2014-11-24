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

  def writeEncoded(): Unit = {
    val mimickedCodeTree = Huffman.createCodeTree(xs)(Huffman.singleton)

    val inputCharList = inputText.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)

    val msg = Huffman.decode(mimickedCodeTree, mimickerBits).mkString
    val txtStatistics = Huffman.charByWeightAndEncoding(xs)
    encodedText.text = msg + "\n\n" + txtStatistics
    encodedText.caret.position = 0
  }

  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      plainText.caret.position = 0

      xs = lines.mkString.trim.filter(c => c.isLetterOrDigit || c == ' ').toLowerCase.toList
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
