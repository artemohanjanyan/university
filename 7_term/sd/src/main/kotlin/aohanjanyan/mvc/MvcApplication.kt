package aohanjanyan.mvc

import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication

@SpringBootApplication
class MvcApplication

fun main(args: Array<String>) {
    SpringApplication.run(MvcApplication::class.java, *args)
}
