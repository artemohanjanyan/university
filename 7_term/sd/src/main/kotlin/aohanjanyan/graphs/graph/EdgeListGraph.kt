package aohanjanyan.graphs.graph

class EdgeListEdge(val from: Int, val to: Int) {
    fun other(v: Int): Int =
            if (from == v) to else from

    fun incident(v: Int): Boolean = from == v || to == v
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

    override fun removeEdge(edge: EdgeListEdge) {
        edges.remove(edge)
    }
}