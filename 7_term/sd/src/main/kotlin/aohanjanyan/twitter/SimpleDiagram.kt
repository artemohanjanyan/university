package aohanjanyan.twitter

fun makeDiagram(hours: List<Int>, forHours: Int): Array<Int> {
    val diagram = Array(forHours, { 0 })
    for (hour in hours) {
        if (hour < forHours) {
            ++diagram[hour]
        }
    }
    return diagram
}