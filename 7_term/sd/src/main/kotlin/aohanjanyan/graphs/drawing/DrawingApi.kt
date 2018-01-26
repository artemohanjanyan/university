package aohanjanyan.graphs.drawing

interface DrawingApi {
    fun getDrawingAreaWidth(): Int

    fun getDrawingAreaHeight(): Int

    fun drawCircle(center: Point, r: Int)

    fun drawLine(start: Point, end: Point)
}