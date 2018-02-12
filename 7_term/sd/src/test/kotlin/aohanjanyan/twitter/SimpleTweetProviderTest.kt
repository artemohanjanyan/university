package aohanjanyan.twitter

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.mockito.Mockito.*
import twitter4j.QueryResult
import twitter4j.Status
import twitter4j.Twitter
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.*


internal class SimpleTweetProviderTest {

    private val someLocalDateTime = LocalDateTime.now()
    lateinit private var tweetProvider: TweetProvider

    @BeforeEach
    fun setUp() {
        val tweets = mock(Status::class.java).also {
            val date = Date.from(
                    someLocalDateTime.atZone(ZoneId.systemDefault()).toInstant()
            )
            `when`(it.createdAt).thenReturn(date)
        }

        val twitterResponse: List<Status> = arrayListOf(tweets)

        val queryResult = mock(QueryResult::class.java).also {
            `when`(it.tweets).thenReturn(twitterResponse)

            `when`(it.nextQuery()).thenReturn(null)
        }

        val twitter = mock(Twitter::class.java).also {
            `when`(it.search(any()))
                    .thenReturn(queryResult)
                    .thenReturn(null)
        }

        tweetProvider = SimpleTweetProvider(twitter)
    }

    @Test
    fun getTweetsByHashtag() {
        val tweets = tweetProvider.getTweetsByHashtag("any")
        assertEquals(1, tweets.size)
        assertEquals(someLocalDateTime, tweets[0].getDate())
    }
}