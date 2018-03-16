package aohanjanyan.calc

class CalcVisitor : TokenVisitor {
    private val stack = ArrayList<Int>()

    override fun visit(token: BracketToken) {
        throw IllegalArgumentException("RPN shouldn't contain brackets")
    }

    override fun visit(token: NumberToken) {
        stack.add(token.n)
    }

    override fun visit(token: OperationToken) {
        if (stack.size < 2) {
            throw IllegalArgumentException("bad RPN sequence")
        }
        val result = token.operation(stack[stack.size - 2], stack[stack.size - 1])
        stack.removeAt(stack.size - 1)
        stack.removeAt(stack.size - 1)
        stack.add(result)
    }

    override fun finish() {
        if (stack.size != 1) {
            throw IllegalArgumentException("bad RPN sequence")
        }
    }

    fun getResult(): Int = stack[0]
}