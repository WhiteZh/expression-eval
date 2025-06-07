package app.expression_eval.ast

import app.expression_eval.ParseError
import app.expression_eval.token.Token

object ExpressionParser extends Parser[ExpressionNode]:

    import ExpressionNode.{BinaryOperatorNodeParser, BracketNodeParser, NumberNodeParser, UnaryOperatorNodeParser}

    private val parsers = List(NumberNodeParser,
                               UnaryOperatorNodeParser,
                               BinaryOperatorNodeParser,
                               BracketNodeParser)

    override def parse(tokens: List[Token]): Either[ParseError, ExpressionNode] =
        parsers.iterator
               .map { parser => parser.parse(tokens) }
               .collectFirst:
                   case Right(node) => node
        match
            case Some(node) => Right(node)
            case None       => Left(ParseError.NoMatch)