package aohanjanyan.twitter

import twitter4j.TwitterFactory
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.util.*

fun main(args: Array<String>) {
    val tweetProvider = SimpleTweetProvider(TwitterFactory().instance)
    val tweets = tweetProvider.getTweetsByHashtag("pixel")

    val now = LocalDateTime.now()
    val tweetsByHours = tweets
            .map { it.getDate().until(now, ChronoUnit.HOURS).toInt() }
    val diagram = makeDiagram(tweetsByHours, 24)

    println(Arrays.toString(diagram))
}