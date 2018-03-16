package aohanjanyan.calc

interface TokenizerState {
    fun processInput(tokenizer: Tokenizer, c: Char)

    fun processEof(tokenizer: Tokenizer) {
        tokenizer.tokenizerState = EofTokenizerState
    }
}

object StartTokenizerState : TokenizerState {
    override fun processInput(tokenizer: Tokenizer, c: Char) {
        when (c) {
            '(' -> tokenizer.tokens.add(LeftBracket)
            ')' -> tokenizer.tokens.add(RightBracket)
            '+' -> tokenizer.tokens.add(PlusToken)
            '-' -> tokenizer.tokens.add(MinusToken)
            '*' -> tokenizer.tokens.add(MulToken)
            '/' -> tokenizer.tokens.add(DivToken)
            in '0'..'9' -> {
                tokenizer.tokenizerState = NumberTokenizerState()
                tokenizer.processInput(c)
            }
            else -> {
                if (!c.isWhitespace()) {
                    throw IllegalArgumentException("unexpected char $c")
                }
            }
        }
    }
}

class NumberTokenizerState : TokenizerState {
    private var number = 0

    override fun processInput(tokenizer: Tokenizer, c: Char) {
        when (c) {
            in '0'..'9' -> {
                number = number * 10 + (c - '0')
            }
            else -> {
                tokenizer.tokens.add(NumberToken(number))
                tokenizer.tokenizerState = StartTokenizerState
                tokenizer.processInput(c)
            }
        }
    }

    override fun processEof(tokenizer: Tokenizer) {
        tokenizer.tokens.add(NumberToken(number))
        super.processEof(tokenizer)
    }
}

object EofTokenizerState : TokenizerState {
    override fun processInput(tokenizer: Tokenizer, c: Char) {
        throw UnsupportedOperationException("operation on eof state")
    }

    override fun processEof(tokenizer: Tokenizer) {
        throw UnsupportedOperationException("operation on eof state")
    }
}

class Tokenizer {
    var tokenizerState: TokenizerState = StartTokenizerState

    val tokens = ArrayList<Token>()

    fun processInput(c: Char) {
        tokenizerState.processInput(this, c)
    }

    fun processEof() {
        tokenizerState.processEof(this)
    }

    fun processInput(input: String) {
        input.forEach(this::processInput)
        processEof()
    }
}

fun tokenize(input: String): List<Token> {
    val tokenizer = Tokenizer()
    tokenizer.processInput(input)
    return tokenizer.tokens
}