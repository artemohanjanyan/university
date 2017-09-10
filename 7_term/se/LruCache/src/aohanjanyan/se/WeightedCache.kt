package aohanjanyan.se

interface WeightedCache<in Key, Value> : Cache<Key, Value> {

    fun put(key: Key, value: Value, weight: Int)
}