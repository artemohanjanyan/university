package aohanjanyan.clock

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.time.Clock
import java.time.ZoneId

internal class EventStatisticsImplTest {

    @Test
    fun simpleTest() {
        val clock = MutableClock(Clock.fixed(
                Clock.systemDefaultZone().instant(),
                ZoneId.systemDefault())
        )
        val eventStatistics = EventStatisticsImpl(clock)

        fun check(total: Double, a: Double, b: Double) {
            val delta = 1e-6
            assertEquals(total / 60, eventStatistics.getAllEventStatistic(), delta)
            assertEquals(a / 60, eventStatistics.getEventStatisticByName("a"), delta)
            assertEquals(b / 60, eventStatistics.getEventStatisticByName("b"), delta)
        }

        eventStatistics.incEvent("a")
        clock.sleep(30)
        check(1.0, 1.0, 0.0)
        eventStatistics.incEvent("b")
        clock.sleep(29)
        check(2.0, 1.0, 1.0)
        clock.sleep(2)
        check(1.0, 0.0, 1.0)
        clock.sleep(30)
        check(0.0, 0.0, 0.0)
    }
}