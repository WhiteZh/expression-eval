package app.expression_eval

enum Token:
    case Number(value: NumericType)
    case OperatorPlus
    case OperatorMinus
    case OperatorTimes
    case OperatorDiv
    case BracketStart
    case BracketEnd
