package app.expression_eval.token

import app.expression_eval.NumericType

enum Token:
    case Number(value: NumericType)
    case Plus
    case Minus
    case Times
    case Div
    case BracketStart
    case BracketEnd
