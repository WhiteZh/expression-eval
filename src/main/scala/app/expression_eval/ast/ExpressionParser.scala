package app.expression_eval.ast

import app.expression_eval.token.Token
import app.expression_eval.{NumericType, ParseError}
import lib.util.ExplicitStatePassing.*

import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.*

object ExpressionParser extends Parser[ExpressionNode]:
    private enum Element:
        case Number(value: NumericType)
        case OperatorPlus
        case OperatorMinus
        case OperatorTimes
        case OperatorDiv
        case Bracket(elements: List[Element])

    private object Element:
        type UnaryOpType = OperatorPlus.type | OperatorMinus.type
        type BinaryOpType = OperatorPlus.type | OperatorMinus.type | OperatorTimes.type | OperatorDiv.type

    private object ElementListParser:

        private val MissingEnclosedParenthesis = ParseError.SyntaxError("missing enclosed parenthesis")

        private type OneToOneToken = Token.Number |
                                     Token.Plus.type |
                                     Token.Minus.type |
                                     Token.Times.type |
                                     Token.Div.type

        extension (token: OneToOneToken)
            private def toElement: Element = token match
                case Token.Number(x) => Element.Number(x)
                case Token.Plus      => Element.OperatorPlus
                case Token.Minus     => Element.OperatorMinus
                case Token.Times     => Element.OperatorTimes
                case Token.Div       => Element.OperatorDiv

        private def partialParse
            (buffer: ListBuffer[Element], tokens: List[Token])
            (insideBracket: Boolean)
        : TailRec[Either[ParseError, (ListBuffer[Element], List[Token])]] =
            tokens match
                case (token: OneToOneToken) :: resTokens => tailcall(partialParse(buffer :+ token.toElement, resTokens)(insideBracket))

                case Token.BracketStart :: resTokens =>
                    partialParse(ListBuffer.empty, resTokens)(true).result match
                        case Left(x)                       => done(Left(x))
                        case Right(innerBuffer, resTokens) => tailcall(partialParse(buffer :+ Element.Bracket(innerBuffer.toList), resTokens)(insideBracket))

                case Token.BracketEnd :: resTokens => done(if insideBracket then Right((buffer, resTokens))
                                                           else Left(ParseError.UnexpectedTokens(Token.BracketEnd :: resTokens)))
                case Nil                           => done(if insideBracket then Left(MissingEnclosedParenthesis)
                                                           else Right((buffer, Nil)))

        def parse(tokens: List[Token]): Either[ParseError, List[Element]] =
            partialParse(ListBuffer.empty, tokens)(false).result.flatMap:
                case (elements, Nil) => Right(elements.toList)
                case (_, tokens)     => Left(ParseError.UnexpectedTokens(tokens))

    private object OperatorChain:
        type OpType = (NumericType, NumericType) => NumericType

        def fromSeq(a: ExpressionNode, seq: Seq[(Int, OpType, ExpressionNode)]): OperatorChain =
            val bodySeq = seq.foldRight(None: Option[Body]):
                (body, next) =>
                    val (precedence: Int, op: OpType, b: ExpressionNode) = body
                    Some(Body(precedence, op, b, next))

            OperatorChain(Head(a, bodySeq))

        private case class Head(a: ExpressionNode, next: Option[Body] = None)

        private case class Body(precedence: Int, op: OpType, b: ExpressionNode, next: Option[Body] = None)

    private class OperatorChain private(private val head: OperatorChain.Head):

        import OperatorChain.{Body, Head}

        def toNode: ExpressionNode = head.next match
            case None       => head.a
            case Some(next) => stateLoop(head.a, next):
                (a, body) =>
                    body.next match
                        case None       => returnResult(ExpressionNode.BinaryOperatorNode(body.op, a, body.b))
                        case Some(next) => if body.precedence >= next.precedence
                                           then updateState(ExpressionNode.BinaryOperatorNode(body.op, a, body.b), next)
                                           else updateState(a, Body(body.precedence,
                                                                    body.op,
                                                                    ExpressionNode.BinaryOperatorNode(next.op, body.b, next.b),
                                                                    next.next))

    private object OperatorChainParser:

        import Element.*

        extension (op: UnaryOpType)
            private def buildNodeWith(value: ExpressionNode): ExpressionNode = op match
                case OperatorPlus  => ExpressionNode.UnaryOperatorNode(identity, value)
                case OperatorMinus => ExpressionNode.UnaryOperatorNode(-_, value)

        private def compressUnaryOps(ops: Iterable[UnaryOpType], value: ExpressionNode): ExpressionNode =
            ops.foldLeft(value)((node, op) => op.buildNodeWith(node))

        private def takeFirstNode(elements: List[Element]): Either[ParseError, (ExpressionNode, List[Element])] =
            stateLoop(ListBuffer.empty: ListBuffer[UnaryOpType], elements) {
                case (buffer, Number(num) :: restList)            => returnResult(Right((compressUnaryOps(buffer, ExpressionNode.NumberNode(num)), restList)))
                case (buffer, Bracket(innerElements) :: restList) => returnResult:
                    val inner = OperatorChainParser.parse(innerElements)
                    inner.map { innerOPC => (compressUnaryOps(buffer, innerOPC.toNode), restList) }
                case (buffer, (unaryOp: UnaryOpType) :: restList) => updateState(buffer :+ unaryOp, restList)
                case (_, _ :: _)                                  => returnResult(Left(ParseError.SyntaxError()))
                case (_, Nil)                                     => returnResult(Left(ParseError.SyntaxError()))
            }

        extension (op: BinaryOpType)
            private def buildBodyLikeWith(node: ExpressionNode): OperatorChain.BodyLike = op match
                case OperatorPlus  => (1, _ + _, node)
                case OperatorMinus => (1, _ - _, node)
                case OperatorTimes => (2, _ * _, node)
                case OperatorDiv   => (2, _ / _, node)

        def parse(elements: List[Element]): Either[ParseError, OperatorChain] =
            takeFirstNode(elements).flatMap:
                (head, tailElements) =>
                    stateLoop(ListBuffer.empty: ListBuffer[OperatorChain.BodyLike], tailElements):
                        case (buffer, Nil)                                => returnResult(Right(OperatorChain.fromSeq(head, Seq.empty)))
                        case (buffer, (op: BinaryOpType) :: tailElements) => takeFirstNode(tailElements) match
                            case Left(e)                   => returnResult(Left(e))
                            case Right(node, Nil)          => returnResult:
                                val bodyLikeList  = (buffer :+ op.buildBodyLikeWith(node)).toList
                                val operatorChain = OperatorChain.fromSeq(head, bodyLikeList)
                                Right(operatorChain)
                            case Right(node, tailElements) => updateState(buffer :+ op.buildBodyLikeWith(node), tailElements)

                        case _ => returnResult(Left(ParseError.SyntaxError()))

    override def parse(tokens: List[Token]): Either[ParseError, ExpressionNode] =
        for
            elementList <- ElementListParser.parse(tokens)
            operatorChain <- OperatorChainParser.parse(elementList)
        yield operatorChain.toNode