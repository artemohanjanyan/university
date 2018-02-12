package aohanjanyan.graphs

import aohanjanyan.graphs.drawing.AwtDrawing
import aohanjanyan.graphs.drawing.JavaFXDrawing
import javafx.application.Application

fun main(args: Array<String>) {
    if (args.size < 2) {
        println("specify API and graph implementation")
        return
    }

    when (args[0]) {
        "fx" -> {
            Application.launch(JavaFXDrawing::class.java, *args)
        }
        "awt" -> AwtDrawing(args).showWindow()
        else -> {
            println("unknown API")
            return
        }
    }
}