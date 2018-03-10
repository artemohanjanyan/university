package aohanjanyan.clock

interface EventsStatistic {
    fun incEvent(name: String)
    fun getEventStatisticByName(name: String): Double
    fun getAllEventStatistic(): Double
    fun printStatistic()
}