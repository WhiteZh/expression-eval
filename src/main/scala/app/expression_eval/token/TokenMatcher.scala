package app.expression_eval.token

import scala.util.Try


// should be eager, that is, keep consuming whitespaces (for this language) even after a successful match
// until further consumption results in failure, that is, no longer a whitespace
trait TokenMatcher extends (LazyList[Char] => Option[(Token, LazyList[Char])])

object TokenMatcher:
    val candidates: List[TokenMatcher] = List(LiteralTokenMatcher("+", Token.Plus),
                                              LiteralTokenMatcher("-", Token.Minus),
                                              LiteralTokenMatcher("*", Token.Times),
                                              LiteralTokenMatcher("/", Token.Div),
                                              LiteralTokenMatcher("(", Token.BracketStart),
                                              LiteralTokenMatcher(")", Token.BracketEnd),
                                              NumberTokenMatcher)
    
    class LiteralTokenMatcher(val literal: List[Char], val token: Token) extends TokenMatcher:
        def this(literal: String, token: Token) = this(literal.toList, token)
        
        override def apply(buffer: LazyList[Char]): Option[(Token, LazyList[Char])] =
            val (firstHalf, secondHalf) = buffer.splitAt(literal.length)
            if firstHalf.length < literal.length then None
            else if !firstHalf.zip(literal).forall(_ == _) then None
                 else Some(token, secondHalf.dropWhile(_.isWhitespace))

    object NumberTokenMatcher extends TokenMatcher:
        override def apply(buffer: LazyList[Char]): Option[(Token, LazyList[Char])] =
            val (chunk, restBuffer) = buffer.span(c => c.isDigit || c == '.')
            Try(chunk.mkString.toDouble).toOption.map { x => (Token.Number(x), restBuffer) }