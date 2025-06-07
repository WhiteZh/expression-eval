package app.expression_eval.ast

import app.expression_eval.NumericType
import app.expression_eval.token.Token
import lib.util.ExplicitStatePassing.*

trait ExpressionNode:
    def evaluate: NumericType

object ExpressionNode:
    case class NumberNode(x: NumericType) extends ExpressionNode:
        override def evaluate: NumericType = x

    object NumberNodeParser extends PartialParser[NumberNode]:
        override def partialParse(tokens: List[Token]): Option[(NumberNode, List[Token])] =
            tokens match
                case Token.Number(x) :: restTokens => Some(NumberNode(x), restTokens)
                case _                             => None

    private type UnaryFunc = NumericType => NumericType

    case class UnaryOperatorNode(op: UnaryFunc,
                                 a:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate)

    object UnaryOperatorNodeParser extends PartialParser[UnaryOperatorNode]:
        private enum UnaryToken(val unaryFunction: UnaryFunc):
            case Plus extends UnaryToken(a => a)
            case Minus extends UnaryToken(a => -a)

        private object UnaryToken:
            def unapply(token: Token): Option[UnaryToken] = token match
                case Token.Plus  => Some(Plus)
                case Token.Minus => Some(Minus)
                case _           => None

            def unapply(unaryToken: UnaryToken): UnaryFunc = unaryToken.unaryFunction

        private val aNodeParsers = List(NumberNodeParser,
                                        UnaryOperatorNodeParser,
                                        BracketNodeParser)

        override def partialParse(tokens: List[Token]): Option[(UnaryOperatorNode, List[Token])] =
            tokens match
                case UnaryToken(unaryToken) :: restTokens =>
                    aNodeParsers.iterator
                                .map { parser => parser.partialParse(restTokens) }
                                .collectFirst:
                                    case Some(aNode, restTokens) => (UnaryOperatorNode(unaryToken.unaryFunction, aNode), restTokens)

                case _ => None

    private type BinaryFunc = (NumericType, NumericType) => NumericType

    case class BinaryOperatorNode(op: BinaryFunc,
                                  a:  ExpressionNode,
                                  b:  ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = op(a.evaluate, b.evaluate)

    object BinaryOperatorNodeParser extends PartialParser[BinaryOperatorNode]:
        private enum BinaryToken(val precedence: Int, val binaryFunction: BinaryFunc):
            case Plus extends BinaryToken(1, (a, b) => a + b)
            case Minus extends BinaryToken(1, (a, b) => a - b)
            case Times extends BinaryToken(2, (a, b) => a * b)
            case Div extends BinaryToken(2, (a, b) => a / b)

        private object BinaryToken:
            def unapply(token: Token): Option[BinaryToken] = token match
                case Token.Plus  => Some(Plus)
                case Token.Minus => Some(Minus)
                case Token.Times => Some(Times)
                case Token.Div   => Some(Div)
                case _           => None

            def unapply(binaryToken: BinaryToken): (Int, BinaryFunc) =
                (binaryToken.precedence, binaryToken.binaryFunction)

        private val operandParsers = List(NumberNodeParser,
                                          UnaryOperatorNodeParser,
                                          BracketNodeParser)

        private def nextOperand(tokens: List[Token]): Option[(ExpressionNode, List[Token])] =
            operandParsers.iterator
                          .map { parser => parser.partialParse(tokens) }
                          .collectFirst:
                              case Some(result) => result

        private def nextOperatorAndOperand(tokens: List[Token]): Option[(BinaryToken, ExpressionNode, List[Token])] =
            tokens match
                case BinaryToken(binaryToken) :: restTokens =>
                    nextOperand(restTokens).map { (operand, restTokens) => (binaryToken, operand, restTokens) }

                case _ => None

        override def partialParse(tokens: List[Token]): Option[(BinaryOperatorNode, List[Token])] =
            for
                (headOperand, restTokens) <- nextOperand(tokens)
                (binaryToken, nextOperand, restTokens) <- nextOperatorAndOperand(restTokens)
                result <- stateLoop(headOperand: ExpressionNode, binaryToken: BinaryToken, nextOperand: ExpressionNode, restTokens):
                    case (headOperand, binaryToken, nextOperand, Nil) =>
                        returnResult:
                            Some(BinaryOperatorNode(binaryToken.binaryFunction, headOperand, nextOperand), Nil)

                    case (headOperand, binaryToken, nextOperand, restTokens) =>
                        nextOperatorAndOperand(restTokens) match
                            case Some(secondaryBinaryToken, secondaryNextOperand, restTokens) =>
                                if secondaryBinaryToken.precedence > binaryToken.precedence then
                                    updateState(headOperand,
                                                binaryToken,
                                                BinaryOperatorNode(secondaryBinaryToken.binaryFunction,
                                                                   nextOperand,
                                                                   secondaryNextOperand): ExpressionNode,
                                                restTokens)
                                else
                                    updateState(BinaryOperatorNode(binaryToken.binaryFunction,
                                                                   headOperand,
                                                                   nextOperand),
                                                secondaryBinaryToken,
                                                secondaryNextOperand,
                                                restTokens)

                            case None =>
                                returnResult:
                                    Some(BinaryOperatorNode(binaryToken.binaryFunction, headOperand, nextOperand), restTokens)
            yield result

    case class BracketNode(node: ExpressionNode) extends ExpressionNode:
        override def evaluate: NumericType = node.evaluate

    object BracketNodeParser extends PartialParser[BracketNode]:
        private val innerBracketParsers: List[PartialParser[ExpressionNode]] = List(NumberNodeParser,
                                                                                    UnaryOperatorNodeParser,
                                                                                    BinaryOperatorNodeParser,
                                                                                    BracketNodeParser)

        override def partialParse(tokens: List[Token]): Option[(BracketNode, List[Token])] = tokens match
            case Token.BracketStart :: restTokens =>
                innerBracketParsers
                    .iterator
                    .map { parser => parser.partialParse(restTokens) }
                    .collectFirst:
                        case Some(innerBracket, Token.BracketEnd :: restTokens) =>
                            (BracketNode(innerBracket), restTokens)

            case _ => None