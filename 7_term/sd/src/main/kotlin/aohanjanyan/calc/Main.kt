package aohanjanyan.calc

fun main(args: Array<String>) {
    val input = readLine()
    if (input == null) {
        println("no input")
        return
    }

    val tokens = tokenize(input)

    val printVisitor = PrintVisitor()
    printVisitor.visit(tokens)

    val parseVisitor = ParserVisitor()
    parseVisitor.visit(tokens)
    printVisitor.visit(parseVisitor.tokens)

    val calcVisitor = CalcVisitor()
    calcVisitor.visit(parseVisitor.tokens)
    println(calcVisitor.getResult())
}