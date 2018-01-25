package aohanjanyan.mvc.controller

import aohanjanyan.mvc.data.TodoDao
import org.springframework.stereotype.Controller
import org.springframework.ui.ModelMap
import org.springframework.web.bind.annotation.ModelAttribute
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.servlet.mvc.support.RedirectAttributes

@Controller
class TodoController(val todoDao: TodoDao) {

    @RequestMapping("/")
    fun homePage(): String = "redirect:/get-lists"

    @RequestMapping("get-lists")
    fun getLists(model: ModelMap): String {
        val taskLists = todoDao.getTaskLists()
        model["taskLists"] = taskLists
        return "get-lists"
    }

    @RequestMapping("get-list")
    fun getList(@RequestParam id: Int?, model: ModelMap): String {
        val taskList = todoDao.getTaskListById(id!!)!!
        model["taskList"] = taskList
        return "get-list"
    }

    class AddTaskListCommand {
        var title: String = ""
    }

    @RequestMapping("add-task-list", method = [RequestMethod.POST])
    fun addTaskList(@ModelAttribute addTaskListCommand: AddTaskListCommand,
                    redirectAttributes: RedirectAttributes): String {
        val taskList = todoDao.addTaskList(addTaskListCommand.title)
        redirectAttributes.addAttribute("id", taskList.id)
        return "redirect:/get-list"
    }

    class DeleteTaskListCommand {
        var taskListId: Int = 0
    }

    @RequestMapping("delete-task-list", method = [RequestMethod.POST])
    fun deleteTaskList(@ModelAttribute deleteTaskListCommand: DeleteTaskListCommand): String {
        todoDao.getTaskListById(deleteTaskListCommand.taskListId)?.let(todoDao::deleteTaskList)
        return "redirect:/"
    }

    class DoTaskCommand {
        var taskListId: Int = 0
        var taskI: Int = 0
    }

    @RequestMapping("do-task", method = [RequestMethod.POST])
    fun doTask(@ModelAttribute doTaskCommand: DoTaskCommand,
               redirectAttributes: RedirectAttributes): String {

        todoDao.getTaskListById(doTaskCommand.taskListId)!!.doTask(doTaskCommand.taskI)

        redirectAttributes.addAttribute("id", doTaskCommand.taskListId)
        return "redirect:/get-list"
    }

    class AddTaskCommand {
        var taskListId: Int = 0
        var taskStr: String = ""
    }

    @RequestMapping("add-task", method = [RequestMethod.POST])
    fun addTask(@ModelAttribute addTaskCommand: AddTaskCommand,
                redirectAttributes: RedirectAttributes): String {

        todoDao.getTaskListById(addTaskCommand.taskListId)!!.addTask(addTaskCommand.taskStr)

        redirectAttributes.addAttribute("id", addTaskCommand.taskListId)
        return "redirect:/get-list"
    }
}