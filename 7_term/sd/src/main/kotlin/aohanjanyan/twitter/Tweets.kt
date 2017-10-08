package aohanjanyan.twitter

import java.time.LocalDateTime

interface Tweet {
    fun getDate(): LocalDateTime
}

interface TweetProvider {
    fun getTweetsByHashtag(hashtag: String): List<Tweet>
}