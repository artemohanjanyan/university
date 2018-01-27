package aohanjanyan.graphs.drawing

import aohanjanyan.graphs.graph.Graph
import java.awt.Frame
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.geom.Ellipse2D

class AwtDrawing(val args: Array<String>) : Frame(), DrawingApi {

    override val drawingAreaWidth: Int = 600
    override val drawingAreaHeight: Int = 400

    private lateinit var graphics2D: Graphics2D

    override fun drawCircle(center: Point, r: Int) {
        graphics2D.fill(Ellipse2D.Float(
                (center.x - r).toFloat(), (center.y - r).toFloat(),
                (r * 2).toFloat(), (r * 2).toFloat()))
    }

    override fun drawLine(start: Point, end: Point) {
        graphics2D.drawLine(start.x, start.y, end.x, end.y)
    }

    fun showWindow() {
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent?) {
                System.exit(0)
            }
        })
        setSize(drawingAreaWidth, drawingAreaHeight)
        isVisible = true
    }

    override fun paint(g: Graphics?) {
        super.paint(g)
        graphics2D = g as Graphics2D
        graphics2D.clearRect(0, 0, width, height)
        val graph: Graph<*, *> = makeRandomGraph(args[1])!!
        drawGraph(graph, this)
        isResizable = false
    }
}