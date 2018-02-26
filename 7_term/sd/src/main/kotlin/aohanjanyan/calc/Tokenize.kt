package aohanjanyan.calc

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