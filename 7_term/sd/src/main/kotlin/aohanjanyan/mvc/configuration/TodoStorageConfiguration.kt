package aohanjanyan.mvc.configuration

import aohanjanyan.mvc.data.InMemoryTodo
import aohanjanyan.mvc.data.TodoDao
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class TodoStorageConfiguration {
    @Bean
    fun todo(): TodoDao = InMemoryTodo().apply {
        addTaskList("do hw").run {
            addTask("do hw1")
            addTask("do hw2")
            addTask("do hw3")
            doTask(0)
        }

        addTaskList("make dinner").run {
            addTask("find recipe")
            addTask("buy products")
            addTask("cook")
            doTask(1)
        }
    }
}