package aohanjanyan.calc

interface TokenVisitor {
    fun visit(tokens: List<Token>) {
        tokens.forEach {
            it.accept(this)
        }
        finish()
    }

    fun visit(token: BracketToken)
    fun visit(token: NumberToken)
    fun visit(token: OperationToken)
    fun finish()
}

sealed class Token {
    abstract fun accept(tokenVisitor: TokenVisitor)
}

sealed class BracketToken : Token() {
    override fun accept(tokenVisitor: TokenVisitor) {
        tokenVisitor.visit(this)
    }
}

sealed class EvalToken : Token()

sealed class OperationToken(val priority: Int, val operation: (Int, Int) -> Int) :
        EvalToken() {
    override fun accept(tokenVisitor: TokenVisitor) {
        tokenVisitor.visit(this)
    }
}

object LeftBracket : BracketToken()
object RightBracket : BracketToken()

data class NumberToken(val n: Int) : EvalToken() {
    override fun accept(tokenVisitor: TokenVisitor) {
        tokenVisitor.visit(this)
    }
}

object PlusToken : OperationToken(0, Int::plus)
object MinusToken : OperationToken(0, Int::plus)
object DivToken : OperationToken(1, Int::div)
object MulToken : OperationToken(1, Int::times)