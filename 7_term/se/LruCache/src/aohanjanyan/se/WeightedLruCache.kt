package aohanjanyan.se

class WeightedLruCache<in Key, Value>(private val maxWeight: Int) : WeightedCache<Key, Value> {

    private class ListData<out Key, Value>(val key: Key, var value: Value, var weight: Int)

    private val linkedList: LinkedList<ListData<Key, Value>> = LinkedList()
    private val pointerMap: MutableMap<Key, LinkedList.Pointer<ListData<Key, Value>>> = HashMap()
    private var totalWeight: Int = 0

    init {
        if (maxWeight <= 0) {
            throw IllegalArgumentException("maxWeight should be positive")
        }
    }

    override fun put(key: Key, value: Value) {
        put(key, value, 1)
    }

    override fun get(key: Key): Value? = pointerMap[key]
            ?.apply { moveToHead() }
            ?.value()
            ?.value

    override fun put(key: Key, value: Value, weight: Int) {
        if (weight > maxWeight) {
            throw IllegalArgumentException("weight can't be greater than maxWeight")
        }

        val existing = pointerMap[key]
        if (existing != null) {
            existing.moveToHead()
            if (existing.value().weight != weight) {
                totalWeight -= existing.value().weight
                totalWeight += weight
                existing.value().weight = weight
            }
        } else {
            linkedList.addToHead(ListData(key, value, weight))
            pointerMap[key] = linkedList.head()!!
            totalWeight += weight
        }

        dropExcess()
        assert(linkedList.size() == pointerMap.size,
                { "size of list != size of map" })
    }

    private fun dropExcess() {
        while (totalWeight > maxWeight) {
            assert(linkedList.tail() != null, { "positive weight in empty cache" })

            linkedList.tail()?.apply {
                totalWeight -= value().weight
                pointerMap.remove(value().key)
                remove()
            }
        }
    }
}