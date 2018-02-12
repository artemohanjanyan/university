package aohanjanyan.graphs.drawing

import aohanjanyan.graphs.graph.AdjacencyListGraph
import aohanjanyan.graphs.graph.AdjacencyMatrixGraph
import aohanjanyan.graphs.graph.EdgeListGraph
import aohanjanyan.graphs.graph.Graph
import java.util.*

fun <V, E> inflateGraphWithRate(graph: Graph<V, E>, rate: Double, random: Random) {
    val processedPairs = HashSet<Pair<V, V>>()
    for (v1 in graph.vertices) {
        for (v2 in graph.vertices) {
            if (!processedPairs.contains(Pair(v2, v1))) {
                processedPairs.add(Pair(v1, v2))
                if (random.nextDouble() < rate) {
                    graph.addEdge(v1, v2)
                }
            }
        }
    }
}

fun makeRandomGraph(impl: String, seed: Long? = null): Graph<*, *>? {
    val random = seed?.let { Random(it) } ?: Random()

    val graphSize = 5 + random.nextInt(16)
    val graph: Graph<*, *> = when (impl) {
        "adjacencyList" -> AdjacencyListGraph(graphSize)
        "matrix" -> AdjacencyMatrixGraph(graphSize)
        "edgeList" -> EdgeListGraph(graphSize)
        else -> {
            println("unknown graph implementation")
            return null
        }
    }

    val edgeRate = (random.nextInt(9) + 1) * 0.1
    inflateGraphWithRate(graph, edgeRate, random)

    return graph
}

fun <V, E> drawGraph(graph: Graph<V, E>, drawingApi: DrawingApi) {
    val indexes = graph.vertices.withIndex().associate { Pair(it.value, it.index) }
    val n = graph.vertices.size

    val center = Point(drawingApi.drawingAreaWidth / 2, drawingApi.drawingAreaHeight / 2)
    val bigR = Math.min(drawingApi.drawingAreaHeight, drawingApi.drawingAreaWidth) / 2 * 0.9
    val vertexR = 20

    fun getVertexCenter(i: Int): Point {
        return Point(
                (center.x + bigR * Math.cos(Math.PI * 2 * i / n)).toInt(),
                (center.y + bigR * Math.sin(Math.PI * 2 * i / n)).toInt()
        )
    }

    for (i in (0 until n)) {
        drawingApi.drawCircle(getVertexCenter(i), vertexR)
    }

    for (edge in graph.edges) {
        val (v1, v2) = graph.getEdgeVertices(edge)
        drawingApi.drawLine(getVertexCenter(indexes[v1]!!), getVertexCenter(indexes[v2]!!))
    }
}
