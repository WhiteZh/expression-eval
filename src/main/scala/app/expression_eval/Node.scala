package app.expression_eval

enum Node:
    case Value(x: NumericType)
    case UnaryOp(op: NumericType => NumericType, a: Node)
    case BinaryOp(op: (NumericType, NumericType) => NumericType, a: Node, b: Node)

    def evaluate: NumericType = this match
        case Value(x) => x
        case UnaryOp(op, a) => op(a.evaluate)
        case BinaryOp(op, a, b) => op(a.evaluate, b.evaluate)