package aohanjanyan.cache

class LinkedList<T> {

    fun addToHead(value: T): Pointer<T> = Node(value)
            .apply { appendAfter(fakeNode) }

    fun head(): Pointer<T>? = fakeNode.next
            .takeIf { it != fakeNode }

    fun tail(): Pointer<T>? = fakeNode.prev
            .takeIf { it != fakeNode }

    fun size(): Int = size

    interface Pointer<out T> {
        fun value(): T

        fun moveToHead()

        fun remove()
    }

    private var size = 0
    private val fakeNode = Node(null).apply {
        prev = this
        next = this
    }

    inner private class Node(var value: T?,
                             var prev: Node? = null,
                             var next: Node? = null) : Pointer<T> {
        override fun value(): T = value!!

        override fun moveToHead() {
            remove()
            appendAfter(fakeNode)
        }

        override fun remove() {
            prev!!.next = next
            next!!.prev = prev
            prev = null
            next = null
            --size
        }

        fun appendAfter(node: Node) {
            next = node.next
            prev = node

            node.next = this
            next!!.prev = this

            ++size
        }
    }

    internal fun checkStructure() {
        val collectToList = { next: (Node) -> Node? ->
            val list = ArrayList<T>()
            var node = next(fakeNode)
            while (node != fakeNode) {
                list.add(node!!.value())
                node = next(node)
            }
            list
        }

        val forwardOrder = collectToList { it.next }
        val reverseOrder = collectToList { it.prev }
        assert(forwardOrder == reverseOrder.reversed())
    }
}