import javax.swing.border.EmptyBorder

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import scala.io.Source
import scala.swing.Orientation._
import java.awt.Color._
object StegaUI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Scala Steganography"

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
    //border = Swing.MatteBorder(15, 10, 10, 10, java.awt.Color.DARK_GRAY)
    //border = new EmptyBorder(8, 8, 8, 8)
    dividerSize = 3
    //peer.setBackground(ivoryBlack)
    peer.setDividerLocation(0.5f)

  }

  val randomSeed = new Button with compColor {
    text = "RND SEED"
    enabled = true
  }
  val inputSeed = new TextField with compColor {
    text = "select substring from loaded text"
    columns = 74
  }

  val inputMsg = new TextField with compColor {
    text = "type msg here, then hit enter"
    columns = 75
    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        writeEncoded()
    }
  }

  val seedLength = new ComboBox(1 to 20) { peer.setSelectedIndex(9) }

  val arrow =  new String(Character.toChars(8594))
  val msgPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label with compColor {text = "MSG "+arrow}, inputMsg) with compColor
  val seedPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label with compColor {text = "SEED "+arrow}, inputSeed, randomSeed, seedLength) with compColor

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

  // takes typed input and translates it into msg
  def getMsg(mimickedCodeTree: Huffman.CodeTree): String = {
    val inputCharList = inputMsg.text.toList
    val mimickerCodeTree = Huffman.createCodeTree(inputCharList)(Huffman.singleton)
    val mimickerBits = Huffman.quickEncode(mimickerCodeTree)(inputCharList)
    Huffman.decode(mimickedCodeTree, mimickerBits).mkString
  }

  def writeEncoded(): Unit = {
    //val txtStatistics = Huffman.charByWeightAndEncoding(xs)

    val sst = Wayner.nthOrderFrequencies(seedLength.selection.item, xs.mkString("\n"))
    val mf: Wayner.MimicFunction = Wayner.createMimicFunction(sst)

    val bits = Wayner.createBitList(inputMsg.text)
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

    encodedText.text = xss.mkString("\n")
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

      xs = lines
    }
  }

  def fileMenu = new Menu("File") {
    contents += new MenuItem(Action("Open") {open()})
    contents += new MenuItem(Action("Exit") {sys.exit(0)})
  }

}
