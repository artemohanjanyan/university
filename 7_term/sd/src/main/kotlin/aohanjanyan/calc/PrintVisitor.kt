package aohanjanyan.calc

class PrintVisitor : TokenVisitor {
    override fun visit(token: BracketToken) {
        if (token == LeftBracket) {
            print("LEFT ")
        } else {
            print("RIGHT ")
        }
    }

    override fun visit(token: NumberToken) {
        print("NUMBER(${token.n}) ")
    }

    override fun visit(token: OperationToken) {
        when (token) {
            PlusToken -> print("PLUS ")
            MinusToken -> print("MINUS ")
            MulToken -> print("MUL ")
            DivToken -> print("DIV ")
        }
    }

    override fun finish() {
        println()
    }
}