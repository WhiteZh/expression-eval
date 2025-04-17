package app.expression_eval.ast

import app.expression_eval.{Error, NumericType}
import app.expression_eval.token.Token

trait Parser[+T]:
    def parse(tokens: List[Token]): Either[Error, T]

object Parser extends Parser[NumericType]:
    override def parse(tokens: List[Token]): Either[Error, NumericType] =
        ExpressionParser.parse(tokens).map(_.evaluate)
