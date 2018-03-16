package aohanjanyan.clock

import java.time.Clock
import java.time.Instant
import java.time.temporal.ChronoUnit.HOURS
import java.util.*
import kotlin.math.max

class EventStatisticsImpl(private val clock: Clock) : EventsStatistic {

    private val eventList: Queue<Pair<String, Instant>> = ArrayDeque()
    private val eventCount: MutableMap<String, Int> = HashMap()
    private var totalCount: Int = 0
    private var currentInstant = clock.instant()

    private fun prepare() {
        currentInstant = clock.instant()
        while (eventList.size > 0
                && HOURS.between(eventList.peek().second, currentInstant) > 0) {
            val (name, _) = eventList.poll()
            val newCount = eventCount[name]!! - 1
            if (newCount > 0) {
                eventCount[name] = newCount
            } else {
                eventCount.remove(name)
            }
            --totalCount
        }
    }

    override fun incEvent(name: String) {
        prepare()
        eventList.add(Pair(name, currentInstant))
        eventCount[name] = eventCount.getOrDefault(name, 0) + 1
        ++totalCount
    }

    override fun getEventStatisticByName(name: String): Double {
        prepare()
        return eventCount.getOrDefault(name, 0) / 60.0
    }

    override fun getAllEventStatistic(): Double {
        prepare()
        return totalCount / 60.0
    }

    override fun printStatistic() {
        prepare()
        println("${getAllEventStatistic()} rmp during the last hour")

        val row1Width = max(
                (eventCount.keys.maxBy { it.length } ?: return).length,
                4
        )

        fun msgWithMargin(msg: String): String {
            return msg + " ".repeat(row1Width - msg.length)
        }

        val header = " ${msgWithMargin("name")}   rpm                  "
        println(header)
        println("-".repeat(header.length))
        for ((name, _) in eventCount) {
            println(" ${msgWithMargin(name)}   ${getEventStatisticByName(name)}")
        }
    }
}