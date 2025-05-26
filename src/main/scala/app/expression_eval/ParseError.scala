package app.expression_eval

import app.expression_eval.token.Token

enum ParseError:
    case NoMatch
    case NonExhaustedTokens(tokens: List[Token])
    case UnexpectedTokens(tokens: List[Token])
    case SyntaxError(message: String = "")