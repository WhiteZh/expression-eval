package app.expression_eval.ast

import app.expression_eval.token.Token

trait PartialParser[+T]:
    def partialParse(tokens: List[Token]): Option[(T, List[Token])]
