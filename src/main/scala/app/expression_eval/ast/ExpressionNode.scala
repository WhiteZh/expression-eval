package app.expression_eval.ast

import app.expression_eval.NumericType

trait ExpressionNode:
    def evaluate: NumericType

object ExpressionNode:
    case class Value(x: NumericType) extends ExpressionNode:
        override def evaluate: NumericType = x

    case class UnaryInfixOperator(op: NumericType => NumericType,
                                  a:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate)
    
    case class BinaryInfixOperator(op: (NumericType, NumericType) => NumericType,
                                   a:  ExpressionNode,
                                   b:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate, b.evaluate)