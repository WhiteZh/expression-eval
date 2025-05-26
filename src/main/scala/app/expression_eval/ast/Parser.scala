package app.expression_eval.ast

import app.expression_eval.{NumericType, ParseError}
import app.expression_eval.token.Token

trait Parser[+T <: ExpressionNode]:
    def parse(tokens: List[Token]): Either[ParseError, T]
