package app.expression_eval.ast

import app.expression_eval
import app.expression_eval.ParseError
import app.expression_eval.token.Token

trait PartialParser[+T <: ExpressionNode] extends Parser[T]:
    def partialParse(tokens: List[Token]): Option[(T, List[Token])]

    override def parse(tokens: List[Token]): Either[ParseError, T] =
        partialParse(tokens) match
            case None => Left(ParseError.NoMatch)
            case Some(node: T, Nil) => Right(node)
            case Some(_, remainingTokens) => Left(ParseError.NonExhaustedTokens(remainingTokens))