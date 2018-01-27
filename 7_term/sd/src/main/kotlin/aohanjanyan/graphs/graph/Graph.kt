package aohanjanyan.graphs.graph

interface Graph<V, E> {
    val vertices: Collection<V>

    val edges: Collection<E>
        get() = vertices
                .flatMap { this.incidentEdges(it) }
                .distinct()

    fun addVertex(): V

    fun removeVertex(vertex: V)

    fun incidentEdges(vertex: V): Collection<E>

    fun addEdge(from: V, to: V): E

    fun getEdgeVertices(edge: E): Pair<V, V>

    fun getEdge(from: V, to: V): E?

    fun removeEdge(edge: E)
}