import scala.swing._
import scala.swing.BorderPanel.Position._
import javax.swing.ScrollPaneConstants._
import scala.swing.FileChooser._
import event._
import scala.io.Source

object StegaUI extends SimpleSwingApplication {

  val chooser = new FileChooser


  val plainText = new TextArea(20, 60) { editable = false }
  val encodedText = new TextArea(20, 60) { editable = false }
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

    //encodedText.text = mimickerBits.toString

    val a = Huffman.decode(mimickedCodeTree, mimickerBits).mkString
    val b = "\n"*5 + Huffman.decodedSecret.mkString
    encodedText.text = a + b + "\n"*5 + Huffman.charByWeightAndEncoding(xs)
    //val encoderFunc = Huffman.quickEncode(mimickedCodeTree)_
    //encodedText.text = encoderFunc(inputText.text.toList).mkString.sliding(32, 32).toList.mkString("\n") + "\n"*5 + Huffman.decodedSecret.mkString
  }

  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      xs = lines.mkString.trim.filter(c => c.isLetterOrDigit || c == ' ').toLowerCase.toList
      writeEncoded()
    }
  }

  def fileMenu = new Menu("File") {
    contents += new MenuItem(Action("Open") { open() })
    contents += new MenuItem(Action("Exit") { sys.exit(0) })
  }

  def top = new MainFrame {
    title = "Scala Steganography"

    menuBar = new MenuBar {
      contents += fileMenu
    }

    case class CustomScroll(txtArea: TextArea) extends ScrollPane(txtArea) {
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }

    contents = new BorderPanel {
      layout(new ScrollPane(plainText)) = East
      layout(new ScrollPane(encodedText)) = West
      layout(CustomScroll(encodedText)) = West
      layout(new ScrollPane(inputText)) = South
    }
  }
}
