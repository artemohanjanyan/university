package aohanjanyan.graphs.drawing

interface DrawingApi {
    val drawingAreaWidth: Int

    val drawingAreaHeight: Int

    fun drawCircle(center: Point, r: Int)

    fun drawLine(start: Point, end: Point)
}