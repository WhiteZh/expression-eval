package app.expression_eval.ast

import app.expression_eval.token.Token

trait Parser:
    type ParseResult

    def parse(tokens: List[Token]): ParseResult
