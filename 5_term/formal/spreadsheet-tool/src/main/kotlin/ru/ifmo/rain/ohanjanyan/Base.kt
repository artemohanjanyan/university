package ru.ifmo.rain.ohanjanyan

data class Student(val name: String,
                   val group: Int,
                   val classScore: Int)

interface ResultsProvider {
    fun getStudents(): List<Student>
    fun getSolvedTasks(group: Int): List<Int>
}

interface TicketService {
    fun makeTicketPage(group: Int, newTasks: IntRange)
    fun makeSummaryPage(groups: List<Pair<Int, IntRange>>)
    fun share(emails: List<String>)
}

data class StudentInfo(val email: String,
                       val group: Int)

interface StudentInfoProvider {
    fun getInfo(): Map<String, StudentInfo>
}
