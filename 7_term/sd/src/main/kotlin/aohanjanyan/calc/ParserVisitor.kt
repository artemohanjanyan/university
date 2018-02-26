package aohanjanyan.calc

class ParserVisitor : TokenVisitor {
    val tokens = ArrayList<EvalToken>()
    private val stack = ArrayList<Token>()

    override fun visit(token: BracketToken) {
        when (token) {
            LeftBracket -> stack.add(token)
            RightBracket -> {
                loop@ while (stack.size > 0) {
                    val lastToken = stack.last()
                    when (lastToken) {
                        LeftBracket -> {
                            stack.removeAt(stack.size - 1)
                            break@loop
                        }
                        RightBracket, is NumberToken -> throw AssertionError()
                        is OperationToken -> {
                            tokens.add(lastToken)
                            stack.removeAt(stack.size - 1)
                        }
                    }
                }
                if (stack.size == 0) {
                    throw IllegalArgumentException("no matching opening bracket")
                }
            }
        }
    }

    override fun visit(token: NumberToken) {
        tokens.add(token)
    }

    override fun visit(token: OperationToken) {
        while (stack.size > 0) {
            val lastToken = stack.last()
            if (lastToken is OperationToken && token.priority <= lastToken.priority) {
                tokens.add(lastToken)
                stack.removeAt(stack.size - 1)
            } else {
                break
            }
        }
        stack.add(token)
    }

    override fun finish() {
        while (stack.size > 0) {
            val lastToken = stack.last()
            if (lastToken is OperationToken) {
                tokens.add(lastToken)
                stack.removeAt(stack.size - 1)
            } else {
                throw IllegalArgumentException("no matching closing bracket")
            }
        }
    }
}