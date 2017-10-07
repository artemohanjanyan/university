package aohanjanyan.cache

interface Cache<in Key, Value> {
    fun put(key: Key, value: Value)
    fun get(key: Key): Value?
}