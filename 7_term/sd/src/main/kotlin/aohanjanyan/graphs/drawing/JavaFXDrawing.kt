package aohanjanyan.graphs.drawing

import aohanjanyan.graphs.graph.Graph
import javafx.application.Application
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color
import javafx.stage.Stage

class JavaFXDrawing : Application(), DrawingApi {

    override val drawingAreaWidth: Int = 600
    override val drawingAreaHeight: Int = 400
    private lateinit var canvas: Canvas
    private lateinit var gc: GraphicsContext

    override fun drawCircle(center: Point, r: Int) {
        gc.fillOval(
                (center.x - r).toDouble(), (center.y - r).toDouble(),
                (r * 2).toDouble(), (r * 2).toDouble())
    }

    override fun drawLine(start: Point, end: Point) {
        gc.strokeLine(start.x.toDouble(), start.y.toDouble(), end.x.toDouble(), end.y.toDouble())
    }

    override fun start(primaryStage: Stage?) {
        primaryStage!!

        canvas = Canvas(drawingAreaWidth.toDouble(), drawingAreaHeight.toDouble())
        gc = canvas.graphicsContext2D!!

        val graph: Graph<*, *> = makeRandomGraph(parameters.raw[1])!!
        drawGraph(graph, this)

        val root = Group()
        root.children.add(canvas)
        primaryStage.scene = Scene(root, Color.WHITE)
        primaryStage.isResizable = false
        primaryStage.show()
    }
}