package aohanjanyan.twitter

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.util.*

fun main(args: Array<String>) {
    val tweets = SimpleTweetProvider().getTweetsByHashtag("pixel")

    val now = LocalDateTime.now()
    val tweetsByHours = tweets
            .map { it.getDate().until(now, ChronoUnit.HOURS).toInt() }
    val diagram = makeDiagram(tweetsByHours, 24)

    println(Arrays.toString(diagram))
}