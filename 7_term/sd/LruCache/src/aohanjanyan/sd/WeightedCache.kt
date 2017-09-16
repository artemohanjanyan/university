package aohanjanyan.sd

interface WeightedCache<in Key, Value> {
    fun put(key: Key, value: Value, weight: Int)
    fun get(key: Key): Value?
}