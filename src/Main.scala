import hevs.graphics.FunGraphics
import java.awt.{AWTEvent, Color, Rectangle, Toolkit}
import java.awt.event.{AWTEventListener, InputEvent, MouseAdapter, MouseEvent, MouseWheelEvent}
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Random

object Main extends App {
  // ----- PARAMETERS -----
  // Logical grid dimensions
  var rows = 50
  var cols = 50
  // Base cell size (in pixels; will be scaled by zoom)
  val baseCellSize = 10.0

  // UI: reserve 4 rows of 40 pixels each (total 160 pixels)
  val uiRows = 4
  val rowHeight = 40
  val uiHeight = uiRows * rowHeight  // 160

  // Zoom and pan (for grid drawing)
  var zoom: Double = 1.0
  var panX: Int = 0
  var panY: Int = 0
  def effectiveCellSize: Double = baseCellSize * zoom

  // ----- BOARD STATE -----
  // Each cell holds an Option[Color]:
  //   None         = dead cell
  //   Some(color)  = live cell (painted with that color)
  var board: Array[Array[Option[Color]]] = Array.ofDim[Option[Color]](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols)
    board(r)(c) = if (Random.nextBoolean()) Some(Color.black) else None

  // ----- UNDO HISTORY -----
  var history: List[Array[Array[Option[Color]]]] = Nil
  def deepCopyBoard(b: Array[Array[Option[Color]]]): Array[Array[Option[Color]]] =
    Array.tabulate(b.length, b(0).length)((r, c) => b(r)(c))
  def recordState(): Unit = {
    history = deepCopyBoard(board) :: history
    if (history.length > 20) history = history.take(20)
  }

  // ----- SIMULATION RULES -----
  // Each cell update uses rules defined as:
  //   survival: neighbor counts that let a live cell survive
  //   birth: neighbor counts that cause a dead cell to become alive.
  var currentRules: (Set[Int], Set[Int]) = (Set(2, 3), Set(3))  // Conway's rules
  val rulePresets: List[(String, (Set[Int], Set[Int]))] = List(
    ("Conway", (Set(2, 3), Set(3))),
    ("HighLife", (Set(2, 3), Set(3, 6))),
    ("Seeds", (Set(), Set(2))),
    ("LifeWithoutDeath", (Set(0,1,2,3,4,5,6,7,8), Set(3)))
  )
  var currentRulesIndex: Int = 0
  def cycleRules(): Unit = {
    currentRulesIndex = (currentRulesIndex + 1) % rulePresets.length
    currentRules = rulePresets(currentRulesIndex)._2
  }

  // ----- SIMULATION CONTROL -----
  var paused: Boolean = false

  // ----- PALETTE & BRUSH -----
  val palette: Array[Color] = Array(Color.black, Color.blue, Color.red, Color.green, Color.orange, Color.magenta)
  var currentColor: Color = palette(0)
  var brushSize: Int = 1  // Allowed range: 1 to 10

  // ----- UI BUTTONS -----
  // Row 1 (y: 5–35): Basic controls and palette.
  val playButton   = new Rectangle(10, 5, 60, 30)
  val pauseButton  = new Rectangle(80, 5, 60, 30)
  val clearButton  = new Rectangle(150, 5, 60, 30)
  // Palette swatches start at x = 220; each 30×30 with 5-pixel gap.
  val paletteStartX = 220
  val paletteSwatchSize = 30
  val paletteGap = 5
  // Row 2 (y: 45–75): Preset patterns and Step.
  val stepButton    = new Rectangle(10, 45, 60, 30)
  val gliderButton  = new Rectangle(80, 45, 60, 30)
  val pulsarButton  = new Rectangle(150, 45, 60, 30)
  val gosperButton  = new Rectangle(220, 45, 60, 30)
  // Row 3 (y: 85–115): Save, Load, Undo, Rules.
  val saveButton    = new Rectangle(10, 85, 60, 30)
  val loadButton    = new Rectangle(80, 85, 60, 30)
  val undoButton    = new Rectangle(150, 85, 60, 30)
  val rulesButton   = new Rectangle(220, 85, 60, 30)
  // Row 4 (y: 125–155): Zoom & Pan.
  val zoomInButton  = new Rectangle(10, 125, 60, 30)
  val zoomOutButton = new Rectangle(80, 125, 60, 30)
  val panUpButton   = new Rectangle(150, 125, 60, 30)
  val panDownButton = new Rectangle(220, 125, 60, 30)
  val panLeftButton = new Rectangle(290, 125, 60, 30)
  val panRightButton= new Rectangle(360, 125, 60, 30)

  // ----- PRESET PATTERN MODE -----
  // When presetMode is defined (e.g. Some("glider")), the next left‑click in the grid will place that pattern.
  var presetMode: Option[String] = None

  // ----- PRESET PATTERN FUNCTIONS -----
  def placeGlider(e: MouseEvent): Unit = {
    // Standard glider: relative offsets
    val offsets = List((0,1), (1,2), (2,0), (2,1), (2,2))
    val col = (((e.getX - panX) / effectiveCellSize).toInt)
    val row = ((((e.getY - uiHeight) - panY) / effectiveCellSize).toInt)
    if (row + 2 < rows && col + 2 < cols) {
      for ((dr, dc) <- offsets)
        board(row + dr)(col + dc) = Some(currentColor)
    }
  }

  def placePulsar(e: MouseEvent): Unit = {
    // Pulsar: offsets for a 17x17 pattern
    val offsets = List(
      (2,4), (2,5), (2,6), (2,10), (2,11), (2,12),
      (7,4), (7,5), (7,6), (7,10), (7,11), (7,12),
      (9,4), (9,5), (9,6), (9,10), (9,11), (9,12),
      (14,4), (14,5), (14,6), (14,10), (14,11), (14,12),
      (4,2), (5,2), (6,2), (10,2), (11,2), (12,2),
      (4,7), (5,7), (6,7), (10,7), (11,7), (12,7),
      (4,9), (5,9), (6,9), (10,9), (11,9), (12,9),
      (4,14), (5,14), (6,14), (10,14), (11,14), (12,14)
    )
    val col = (((e.getX - panX) / effectiveCellSize).toInt)
    val row = ((((e.getY - uiHeight) - panY) / effectiveCellSize).toInt)
    if (row + 14 < rows && col + 14 < cols) {
      for ((dr, dc) <- offsets)
        board(row + dr)(col + dc) = Some(currentColor)
    }
  }

  def placeGosper(e: MouseEvent): Unit = {
    // Gosper Glider Gun: relative offsets
    val offsets = List(
      (5,1), (5,2), (6,1), (6,2),
      (3,13), (3,14), (4,12), (4,16), (5,11), (5,17), (6,11), (6,15), (6,17), (6,18),
      (7,11), (7,17), (8,12), (8,16), (9,13), (9,14),
      (1,25), (2,23), (2,25), (3,21), (3,22), (4,21), (4,22), (5,21), (5,22),
      (6,23), (6,25), (7,25),
      (3,35), (3,36), (4,35), (4,36)
    )
    val col = (((e.getX - panX) / effectiveCellSize).toInt)
    val row = ((((e.getY - uiHeight) - panY) / effectiveCellSize).toInt)
    if (row + 20 < rows && col + 40 < cols) {
      for ((dr, dc) <- offsets)
        board(row + dr)(col + dc) = Some(currentColor)
    }
  }

  // ----- SAVE / LOAD / UNDO FUNCTIONS -----
  val colorToChar: Map[Color, Char] = Map(
    Color.black -> 'K',
    Color.blue -> 'B',
    Color.red -> 'R',
    Color.green -> 'G',
    Color.orange -> 'O',
    Color.magenta -> 'M'
  )
  val charToColor: Map[Char, Color] = colorToChar.map(_.swap)

  def saveBoard(): Unit = {
    recordState()
    val pw = new PrintWriter(new File("board.txt"))
    for (r <- 0 until rows) {
      val line = (0 until cols).map { c =>
        board(r)(c) match {
          case Some(col) => colorToChar.getOrElse(col, '?')
          case None      => '.'
        }
      }.mkString
      pw.println(line)
    }
    pw.close()
    println("Board saved.")
  }

  def loadBoard(): Unit = {
    try {
      recordState()
      val lines = Source.fromFile("board.txt").getLines().toArray
      for (r <- 0 until math.min(rows, lines.length)) {
        val line = lines(r)
        for (c <- 0 until math.min(cols, line.length)) {
          val ch = line.charAt(c)
          board(r)(c) = if (ch == '.') None else charToColor.get(ch)
        }
      }
      println("Board loaded.")
    } catch {
      case ex: Exception => println("Load failed: " + ex.getMessage)
    }
  }

  // ----- UPDATE FUNCTION: updateGridCell -----
  // This function converts screen coordinates (adjusting for pan and zoom)
  // to board coordinates and then paints or erases a square of cells (of side brushSize).
  def updateGridCell(e: MouseEvent, place: Boolean): Unit = {
    val x = e.getX
    val y = e.getY
    if (y < uiHeight) return  // Ignore clicks in UI area
    val eff = effectiveCellSize
    val col = (((x - panX) / eff).toInt)
    val row = ((((y - uiHeight) - panY) / eff).toInt)
    val startRow = row - brushSize / 2
    val startCol = col - brushSize / 2
    for (r <- startRow until (startRow + brushSize);
         c <- startCol until (startCol + brushSize)) {
      if (r >= 0 && r < rows && c >= 0 && c < cols) {
        if (place) board(r)(c) = Some(currentColor) else board(r)(c) = None
      }
    }
  }

  // ----- SIMULATION UPDATE FUNCTION -----
  def nextGeneration(bd: Array[Array[Option[Color]]]): Array[Array[Option[Color]]] = {
    val (survival, birth) = currentRules
    val newBoard = Array.ofDim[Option[Color]](rows, cols)
    for (r <- 0 until rows; c <- 0 until cols) {
      var liveNeighbors = 0
      var neighborColor: Option[Color] = None
      for (dr <- -1 to 1; dc <- -1 to 1 if !(dr == 0 && dc == 0)) {
        val nr = r + dr
        val nc = c + dc
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
          bd(nr)(nc) match {
            case Some(col) =>
              liveNeighbors += 1
              if (neighborColor.isEmpty) neighborColor = Some(col)
            case None =>
          }
        }
      }
      bd(r)(c) match {
        case Some(col) =>
          if (survival.contains(liveNeighbors)) newBoard(r)(c) = Some(col)
          else newBoard(r)(c) = None
        case None =>
          if (birth.contains(liveNeighbors))
            newBoard(r)(c) = neighborColor.orElse(Some(currentColor))
          else newBoard(r)(c) = None
      }
    }
    newBoard
  }

  // ----- CREATE THE FUNGRAPHICS WINDOW -----
  val gridWidth = (cols * effectiveCellSize).toInt
  val finalWindowWidth = math.max(510, gridWidth)
  val gridHeight = (rows * effectiveCellSize).toInt
  val finalWindowHeight = uiHeight + gridHeight
  val fg = new FunGraphics(finalWindowWidth, finalWindowHeight, "Ultimate Game of Life")

  // ----- GLOBAL MOUSE WHEEL LISTENER -----
  Toolkit.getDefaultToolkit.addAWTEventListener(new AWTEventListener {
    override def eventDispatched(event: AWTEvent): Unit = {
      event match {
        case mwe: MouseWheelEvent =>
          brushSize = math.max(1, math.min(10, brushSize - mwe.getWheelRotation))
        case _ =>
      }
    }
  }, AWTEvent.MOUSE_WHEEL_EVENT_MASK)

  // ----- MOUSE & MOTION LISTENERS -----
  fg.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent): Unit = {
      val x = e.getX
      val y = e.getY
      if (y < uiHeight) {
        // UI region: determine which row.
        if (y < rowHeight) { // Row 1
          if (playButton.contains(x, y)) { paused = false }
          else if (pauseButton.contains(x, y)) { paused = true }
          else if (clearButton.contains(x, y)) {
            recordState()
            for (r <- 0 until rows; c <- 0 until cols) board(r)(c) = None
          } else {
            // Check palette swatches in row 1.
            for (i <- palette.indices) {
              val sx = paletteStartX + i * (paletteSwatchSize + paletteGap)
              val swatch = new Rectangle(sx, 5, paletteSwatchSize, paletteSwatchSize)
              if (swatch.contains(x, y)) currentColor = palette(i)
            }
          }
        } else if (y < rowHeight * 2) { // Row 2: Presets & Step.
          if (stepButton.contains(x, y)) {
            recordState()
            board = nextGeneration(board)
          } else if (gliderButton.contains(x, y)) {
            presetMode = Some("glider")
          } else if (pulsarButton.contains(x, y)) {
            presetMode = Some("pulsar")
          } else if (gosperButton.contains(x, y)) {
            presetMode = Some("gosper")
          }
        } else if (y < rowHeight * 3) { // Row 3: Save, Load, Undo, Rules.
          if (saveButton.contains(x, y)) { saveBoard() }
          else if (loadButton.contains(x, y)) { loadBoard() }
          else if (undoButton.contains(x, y)) {
            history match {
              case prev :: tail =>
                board = prev; history = tail
              case Nil => println("No undo history!")
            }
          } else if (rulesButton.contains(x, y)) {
            cycleRules()
            println("Rules changed to: " + rulePresets(currentRulesIndex)._1)
          }
        } else { // Row 4: Zoom & Pan.
          if (zoomInButton.contains(x, y)) { zoom = math.min(5.0, zoom * 1.1) }
          else if (zoomOutButton.contains(x, y)) { zoom = math.max(0.5, zoom / 1.1) }
          else if (panUpButton.contains(x, y)) { panY -= 10 }
          else if (panDownButton.contains(x, y)) { panY += 10 }
          else if (panLeftButton.contains(x, y)) { panX -= 10 }
          else if (panRightButton.contains(x, y)) { panX += 10 }
        }
      } else {
        // In the grid area.
        if (presetMode.nonEmpty) {
          recordState()
          presetMode.get match {
            case "glider" => placeGlider(e)
            case "pulsar" => placePulsar(e)
            case "gosper" => placeGosper(e)
            case _ =>
          }
          presetMode = None
        } else {
          e.getButton match {
            case MouseEvent.BUTTON1 =>
              recordState()
              updateGridCell(e, true)
            case MouseEvent.BUTTON3 =>
              recordState()
              updateGridCell(e, false)
            case _ =>
          }
        }
      }
    }
  })

  fg.addMouseMotionListener(new MouseAdapter() {
    override def mouseDragged(e: MouseEvent): Unit = {
      if (e.getY >= uiHeight && presetMode.isEmpty) {
        val mods = e.getModifiersEx
        if ((mods & InputEvent.BUTTON1_DOWN_MASK) != 0)
          updateGridCell(e, true)
        else if ((mods & InputEvent.BUTTON3_DOWN_MASK) != 0)
          updateGridCell(e, false)
      }
    }
  })

  // ----- MAIN LOOP -----
  while (true) {
    fg.frontBuffer.synchronized {
      fg.clear(Color.white)

      // --- Draw UI ---
      // Row 1:
      fg.setColor(if (!paused) new Color(200, 200, 200) else Color.green)
      fg.drawFillRect(playButton.x, playButton.y, playButton.width, playButton.height)
      fg.setColor(Color.black)
      fg.drawString(playButton.x + 10, playButton.y + 20, "Play")

      fg.setColor(if (paused) new Color(200, 200, 200) else Color.red)
      fg.drawFillRect(pauseButton.x, pauseButton.y, pauseButton.width, pauseButton.height)
      fg.setColor(Color.black)
      fg.drawString(pauseButton.x + 5, pauseButton.y + 20, "Pause")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(clearButton.x, clearButton.y, clearButton.width, clearButton.height)
      fg.setColor(Color.black)
      fg.drawString(clearButton.x + 5, clearButton.y + 20, "Clear")

      for (i <- palette.indices) {
        val sx = paletteStartX + i * (paletteSwatchSize + paletteGap)
        val swatch = new Rectangle(sx, 5, paletteSwatchSize, paletteSwatchSize)
        fg.setColor(palette(i))
        fg.drawFillRect(swatch.x, swatch.y, swatch.width, swatch.height)
        if (palette(i) == currentColor) {
          fg.setColor(Color.black)
          fg.drawRect(swatch.x, swatch.y, swatch.width, swatch.height)
        }
      }
      fg.setColor(Color.black)
      fg.drawString(paletteStartX + palette.length * (paletteSwatchSize + paletteGap) + 10, 25, "Brush: " + brushSize)

      // Row 2:
      fg.setColor(Color.cyan)
      fg.drawFillRect(stepButton.x, stepButton.y, stepButton.width, stepButton.height)
      fg.setColor(Color.black)
      fg.drawString(stepButton.x + 10, stepButton.y + 20, "Step")

      fg.setColor(Color.orange)
      fg.drawFillRect(gliderButton.x, gliderButton.y, gliderButton.width, gliderButton.height)
      fg.setColor(Color.black)
      fg.drawString(gliderButton.x + 2, gliderButton.y + 20, "Glider")

      fg.setColor(Color.pink)
      fg.drawFillRect(pulsarButton.x, pulsarButton.y, pulsarButton.width, pulsarButton.height)
      fg.setColor(Color.black)
      fg.drawString(pulsarButton.x + 2, pulsarButton.y + 20, "Pulsar")

      fg.setColor(new Color(150, 150, 255))
      fg.drawFillRect(gosperButton.x, gosperButton.y, gosperButton.width, gosperButton.height)
      fg.setColor(Color.black)
      fg.drawString(gosperButton.x + 2, gosperButton.y + 20, "Gosper")

      // Row 3:
      fg.setColor(Color.lightGray)
      fg.drawFillRect(saveButton.x, saveButton.y, saveButton.width, saveButton.height)
      fg.setColor(Color.black)
      fg.drawString(saveButton.x + 5, saveButton.y + 20, "Save")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(loadButton.x, loadButton.y, loadButton.width, loadButton.height)
      fg.setColor(Color.black)
      fg.drawString(loadButton.x + 5, loadButton.y + 20, "Load")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(undoButton.x, undoButton.y, undoButton.width, undoButton.height)
      fg.setColor(Color.black)
      fg.drawString(undoButton.x + 5, undoButton.y + 20, "Undo")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(rulesButton.x, rulesButton.y, rulesButton.width, rulesButton.height)
      fg.setColor(Color.black)
      fg.drawString(rulesButton.x + 2, rulesButton.y + 20, "Rules: " + rulePresets(currentRulesIndex)._1)

      // Row 4:
      fg.setColor(Color.lightGray)
      fg.drawFillRect(zoomInButton.x, zoomInButton.y, zoomInButton.width, zoomInButton.height)
      fg.setColor(Color.black)
      fg.drawString(zoomInButton.x + 2, zoomInButton.y + 20, "Zoom+")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(zoomOutButton.x, zoomOutButton.y, zoomOutButton.width, zoomOutButton.height)
      fg.setColor(Color.black)
      fg.drawString(zoomOutButton.x + 2, zoomOutButton.y + 20, "Zoom-")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(panUpButton.x, panUpButton.y, panUpButton.width, panUpButton.height)
      fg.setColor(Color.black)
      fg.drawString(panUpButton.x + 2, panUpButton.y + 20, "Pan↑")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(panDownButton.x, panDownButton.y, panDownButton.width, panDownButton.height)
      fg.setColor(Color.black)
      fg.drawString(panDownButton.x + 2, panDownButton.y + 20, "Pan↓")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(panLeftButton.x, panLeftButton.y, panLeftButton.width, panLeftButton.height)
      fg.setColor(Color.black)
      fg.drawString(panLeftButton.x + 2, panLeftButton.y + 20, "Pan←")

      fg.setColor(Color.lightGray)
      fg.drawFillRect(panRightButton.x, panRightButton.y, panRightButton.width, panRightButton.height)
      fg.setColor(Color.black)
      fg.drawString(panRightButton.x + 2, panRightButton.y + 20, "Pan→")

      // --- Draw the Grid ---
      val eff = effectiveCellSize
      for (r <- 0 until rows; c <- 0 until cols) {
        val drawX = panX + (c * eff).toInt
        val drawY = uiHeight + panY + (r * eff).toInt
        board(r)(c) match {
          case Some(col) =>
            fg.setColor(col)
            fg.drawFillRect(drawX, drawY, eff.toInt, eff.toInt)
          case None =>
            fg.setColor(Color.lightGray)
            fg.drawRect(drawX, drawY, eff.toInt, eff.toInt)
        }
      }
    }

    if (!paused) {
      recordState()
      board = nextGeneration(board)
    }

    fg.syncGameLogic(10)
  }
}
