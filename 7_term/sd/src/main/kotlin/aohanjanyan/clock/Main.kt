package aohanjanyan.clock

import java.time.Clock
import java.time.ZoneId

fun main(args: Array<String>) {
    val clock = MutableClock(Clock.fixed(
            Clock.systemDefaultZone().instant(),
            ZoneId.systemDefault())
    )
    val eventStatistics = EventStatisticsImpl(clock)
    (0 until 4).forEach {
        eventStatistics.incEvent("a")
        eventStatistics.incEvent("a")
        eventStatistics.incEvent("acacacacacaca")
        clock.sleep(19)
    }
    (0 until 4).forEach { clock.sleep(19) }
}