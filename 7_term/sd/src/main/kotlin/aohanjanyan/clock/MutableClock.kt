package aohanjanyan.clock

import java.time.Clock
import java.time.Duration
import java.time.Instant
import java.time.ZoneId

class MutableClock(var internalClock: Clock) : Clock() {

    override fun withZone(zone: ZoneId?): Clock = internalClock.withZone(zone)

    override fun getZone(): ZoneId = internalClock.zone

    override fun instant(): Instant = internalClock.instant()

    fun sleep(minutes: Long) {
        internalClock = Clock.offset(internalClock, Duration.ofMinutes(minutes))
    }
}