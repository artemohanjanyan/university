package aohanjanyan.mvc.data

import java.util.*
import kotlin.collections.LinkedHashMap

class InMemoryTaskList(override val id: Int, override val title: String): TaskListDao {
    private val tasks = ArrayList<Task>()

    override fun getTasks(): Collection<Task> = tasks

    override fun addTask(str: String) {
        tasks.add(Task(tasks.size, str, false))
    }

    override fun doTask(i: Int) {
        val task = tasks[i]
        assert(!task.done)
        tasks[i] = Task(task.id, task.str, true)
    }
}

class InMemoryTodo: TodoDao {
    private val taskLists = LinkedHashMap<Int, TaskListDao>()
    private var idCounter = 0

    override fun getTaskLists(): Collection<TaskListDao> = taskLists.values

    override fun getTaskListById(id: Int): TaskListDao? = taskLists[id]

    override fun addTaskList(title: String): TaskListDao {
        val taskList = InMemoryTaskList(idCounter++, title)
        taskLists[taskList.id] = taskList
        return taskList
    }

    override fun deleteTaskList(taskListDao: TaskListDao) {
        taskLists.remove(taskListDao.id)
    }
}