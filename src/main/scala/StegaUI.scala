import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.FileChooser._
import event._
import scala.io.Source

object StegaUI extends SimpleSwingApplication {

  val chooser = new FileChooser


  val plainText = new TextArea(20, 60) { editable = false }
  val encodedText = new TextArea(20, 60) { editable = false }
  val decodedText = new TextField {
    text = "Enter message here"
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }

  var xs: List[Char] = Nil

  def writeEncoded(): Unit = {
    val codeTree = Huffman.createCodeTree(xs)(Huffman.singleton)
    val encoderFunc = Huffman.quickEncode(codeTree)_
    encodedText.text = encoderFunc(decodedText.text.toList).mkString.sliding(32, 32).toList.mkString("\n") + "\n"*5 + Huffman.decodedSecret.mkString
  }

  def open(): Unit = {
    if (chooser.showOpenDialog(plainText) == FileChooser.Result.Approve) {
      val src = Source.fromFile(chooser.selectedFile)
      val lines = src.getLines().toList
      src.close()

      plainText.text = lines.mkString("\n")
      xs = lines.mkString.trim.toList
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

    contents = new BorderPanel {
      layout(new ScrollPane(plainText)) = East
      layout(new ScrollPane(encodedText)) = West
      layout(new ScrollPane(decodedText)) = South
    }
  }
}
