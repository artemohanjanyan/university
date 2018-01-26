package aohanjanyan.graphs.graph

import java.util.*

class AdjacencyListVertex {
    private val edges = ArrayList<AdjacencyListEdge>()

    val incidentEdges: Collection<AdjacencyListEdge> = edges

    fun addEdge(edge: AdjacencyListEdge) {
        edges.add(edge)
    }

    fun removeEdge(edge: AdjacencyListEdge) {
        edges.remove(edge)
    }
}

class AdjacencyListEdge(val from: AdjacencyListVertex, val to: AdjacencyListVertex) {
    fun other(v: AdjacencyListVertex): AdjacencyListVertex =
            if (from == v) to else from
}

class AdjacencyListGraph(initialSize: Int = 0) : Graph<AdjacencyListVertex, AdjacencyListEdge> {
    override val vertices = ArrayList<AdjacencyListVertex>(
            (0 until initialSize).map { AdjacencyListVertex() }
    )

    override fun addVertex(): AdjacencyListVertex {
        val vertex = AdjacencyListVertex()
        vertices.add(vertex)
        return vertex
    }

    override fun removeVertex(vertex: AdjacencyListVertex) {
        for (edge in vertex.incidentEdges) {
            edge.other(vertex).removeEdge(edge)
        }
        vertices.remove(vertex)
    }

    override fun incidentEdges(vertex: AdjacencyListVertex): Collection<AdjacencyListEdge> {
        return vertex.incidentEdges
    }

    override fun addEdge(from: AdjacencyListVertex, to: AdjacencyListVertex): AdjacencyListEdge {
        val edge = AdjacencyListEdge(from, to)
        from.addEdge(edge)
        to.addEdge(edge)
        return edge
    }

    override fun removeEdge(edge: AdjacencyListEdge) {
        edge.from.removeEdge(edge)
        edge.to.removeEdge(edge)
    }
}