package aohanjanyan.twitter

import org.apache.logging.log4j.Level
import org.apache.logging.log4j.LogManager
import twitter4j.Query
import twitter4j.QueryResult
import twitter4j.TwitterFactory
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.*

class SimpleTweetProvider : TweetProvider {

    companion object {
        private val logger = LogManager
                .getLogger(SimpleTweetProvider::class.simpleName)
    }

    private class SimpleTweet(val localDateTime: LocalDateTime)
        : aohanjanyan.twitter.Tweet {

        constructor(date: Date) : this(LocalDateTime.ofInstant(
                date.toInstant(),
                ZoneId.systemDefault()))

        override fun getDate(): LocalDateTime = localDateTime
    }

    override fun getTweetsByHashtag(hashtag: String): List<Tweet> {
        val twitter = TwitterFactory().instance
        var query = Query("#$hashtag")
        query.resultType = Query.ResultType.recent
        query.count = 100

        var queryResult: QueryResult
        var tweetCount = 0
        val tweets = ArrayList<Tweet>()

        while (true) {
            queryResult = twitter.search(query)
            tweetCount += queryResult.tweets.size
            for (tweet in queryResult.tweets) {
                tweets.add(SimpleTweet(tweet.createdAt))
            }
            logger.log(Level.TRACE, "read $tweetCount tweets")

            query = queryResult.nextQuery() ?: break
        }

        return tweets
    }
}