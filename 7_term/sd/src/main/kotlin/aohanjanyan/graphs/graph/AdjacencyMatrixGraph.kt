package aohanjanyan.graphs.graph

typealias MatrixEdge = Pair<Int, Int>

class AdjacencyMatrixGraph(initialSize: Int = 0) : Graph<Int, MatrixEdge> {

    private var matrix = ArrayList<ArrayList<Boolean>>().apply {
        for (i in 0 until initialSize) {
            add(ArrayList((0 until initialSize).map { false }))
        }
    }

    override val vertices get() = (0 until matrix.size).toList()

    override fun addVertex(): Int {
        for (row in matrix) {
            row.add(false)
        }
        matrix.add(ArrayList((0 until matrix.size + 1).map { false }))
        return matrix.size - 1
    }

    override fun removeVertex(vertex: Int) {
        for ((index, row) in matrix.withIndex()) {
            if (index != vertex) {
                row.removeAt(vertex)
            }
        }
        matrix.removeAt(vertex)
    }

    override fun incidentEdges(vertex: Int): Collection<MatrixEdge> {
        return matrix[vertex]
                .withIndex()
                .filter { it.value }
                .map { MatrixEdge(Math.min(vertex, it.index), Math.max(vertex, it.index)) }
    }

    override fun addEdge(from: Int, to: Int): MatrixEdge {
        matrix[from][to] = true
        matrix[to][from] = true
        return MatrixEdge(from, to)
    }

    override fun getEdgeVertices(edge: MatrixEdge): Pair<Int, Int> {
        return edge
    }

    override fun getEdge(from: Int, to: Int): MatrixEdge? {
        if (matrix[from][to]) {
            return Pair(from, to)
        }
        return null
    }

    override fun removeEdge(edge: MatrixEdge) {
        matrix[edge.first][edge.second] = false
        matrix[edge.second][edge.first] = false
    }
}