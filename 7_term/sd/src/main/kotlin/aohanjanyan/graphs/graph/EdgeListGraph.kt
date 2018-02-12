package aohanjanyan.graphs.graph

class EdgeListEdge(from: Int, to: Int) {
    val from: Int = Math.min(from, to)
    val to: Int = Math.max(from, to)

    fun other(v: Int): Int =
            if (from == v) to else from

    fun incident(v: Int): Boolean = from == v || to == v

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as EdgeListEdge

        if (from != other.from) return false
        if (to != other.to) return false

        return true
    }

    override fun hashCode(): Int {
        var result = from
        result = 31 * result + to
        return result
    }
}

class EdgeListGraph(initialSize: Int = 0) : Graph<Int, EdgeListEdge> {
    private var size: Int = initialSize

    override val vertices get() = (0 until size).toList()

    override val edges = ArrayList<EdgeListEdge>()

    override fun addVertex(): Int {
        size += 1
        return size - 1
    }

    override fun removeVertex(vertex: Int) {
        size -= 1

        fun newI(v: Int): Int = if (v >= vertex) v - 1 else v

        edges.removeIf { it.from == vertex || it.to == vertex }
        for (i in edges.indices) {
            val edge = edges[i]
            edges[i] = EdgeListEdge(newI(edge.from), newI(edge.to))
        }
    }

    override fun incidentEdges(vertex: Int): Collection<EdgeListEdge> {
        return edges.filter { it.incident(vertex) }
    }

    override fun addEdge(from: Int, to: Int): EdgeListEdge {
        val edge = EdgeListEdge(from, to)
        edges.add(edge)
        return edge
    }

    override fun getEdgeVertices(edge: EdgeListEdge): Pair<Int, Int> {
        return Pair(edge.from, edge.to)
    }

    override fun getEdge(from: Int, to: Int): EdgeListEdge? {
        val edge = EdgeListEdge(from, to)
        return edges.find { it == edge }
    }

    override fun removeEdge(edge: EdgeListEdge) {
        edges.remove(edge)
    }
}