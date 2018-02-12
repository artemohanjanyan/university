package aohanjanyan.mvc.data

data class Task(val id: Int, val str: String, val done: Boolean)

interface TaskListDao {
    val id: Int

    val title: String

    fun getTasks(): Collection<Task>

    fun addTask(str: String)

    fun doTask(i: Int)
}

interface TodoDao {
    fun getTaskLists(): Collection<TaskListDao>

    fun getTaskListById(id: Int): TaskListDao?

    fun addTaskList(title: String): TaskListDao

    fun deleteTaskList(taskListDao: TaskListDao)
}