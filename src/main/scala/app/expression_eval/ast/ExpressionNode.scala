package app.expression_eval.ast

import app.expression_eval.NumericType

trait ExpressionNode:
    def evaluate: NumericType

object ExpressionNode:
    case class NumberNode(x: NumericType) extends ExpressionNode:
        override def evaluate: NumericType = x

    case class UnaryOperatorNode(op: NumericType => NumericType,
                                 a:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate)

    case class BinaryOperatorNode(op: (NumericType, NumericType) => NumericType,
                                  a:  ExpressionNode,
                                  b:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate, b.evaluate)

    case class BracketNode(node: ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = node.evaluate