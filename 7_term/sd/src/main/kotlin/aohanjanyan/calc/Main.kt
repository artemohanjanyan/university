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

object LeftBracket : BracketToken()
object RightBracket : BracketToken()

sealed class EvalToken : Token()
data class NumberToken(val n: Int) : EvalToken() {
    override fun accept(tokenVisitor: TokenVisitor) {
        tokenVisitor.visit(this)
    }
}

sealed class OperationToken(val priority: Int, val operation: (Int, Int) -> Int) :
        EvalToken() {
    override fun accept(tokenVisitor: TokenVisitor) {
        tokenVisitor.visit(this)
    }
}

object PlusToken : OperationToken(0, Int::plus)
object MinusToken : OperationToken(0, Int::plus)
object MulToken : OperationToken(1, Int::times)
object DivToken : OperationToken(1, Int::div)

fun tokenize(input: String): List<Token> {
    var i = 0
    fun skipSpaces() {
        while (i < input.length && input[i].isWhitespace()) {
            ++i
        }
    }

    val tokens = ArrayList<Token>()

    skipSpaces()
    while (i < input.length) {
        tokens.add(
                when (input[i]) {
                    '(' -> LeftBracket
                    ')' -> RightBracket
                    '+' -> PlusToken
                    '-' -> MinusToken
                    '*' -> MulToken
                    '/' -> DivToken
                    in '0'..'9' -> {
                        var number = 0
                        while (i < input.length && input[i] in '0'..'9') {
                            number = number * 10 + (input[i] - '0')
                            ++i
                        }
                        --i
                        NumberToken(number)
                    }
                    else -> {
                        val message = "bad character ${input[i]} at 1:$i"
                        throw IllegalArgumentException(message)
                    }
                }
        )
        ++i
        skipSpaces()
    }

    return tokens
}

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