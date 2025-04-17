package app.expression_eval.token

import app.expression_eval.Error
import app.expression_eval.token.TokenReader.{Matcher, ParseError}

import scala.util.Try

object TokenReader:
    // should be eager, that is, keep consuming whitespaces (for this language) even after a successful match
    // until further consumption results in failure, that is, no longer a whitespace
    private trait Matcher extends (LazyList[Char] => Option[(Token, LazyList[Char])])

    private object Matcher:
        private class LiteralMatcher(val literal: List[Char], val token: Token) extends Matcher:
            override def apply(buffer: LazyList[Char]): Option[(Token, LazyList[Char])] =
                if buffer.head.isWhitespace then apply(buffer.dropWhile(_.isWhitespace))
                else
                    val (firstHalf, secondHalf) = buffer.splitAt(literal.length)
                    if firstHalf.length < literal.length then None
                    else if !firstHalf.zip(literal).forall(_ == _) then None
                         else Some(token, secondHalf.dropWhile(_.isWhitespace))

        private object NumberMatcher extends Matcher:
            override def apply(buffer: LazyList[Char]): Option[(Token, LazyList[Char])] =
                if buffer.head.isWhitespace then apply(buffer.dropWhile(_.isWhitespace))
                else
                    val (chunk, restBuffer) = buffer.span(c => c.isDigit || c == '.')
                    Try(chunk.mkString.toDouble).toOption.map { x => (Token.Number(x), restBuffer) }

        val candidates: List[Matcher] = List(LiteralMatcher("+".toList, Token.Plus),
                                             LiteralMatcher("-".toList, Token.Minus),
                                             LiteralMatcher("*".toList, Token.Times),
                                             LiteralMatcher("/".toList, Token.Div),
                                             LiteralMatcher("(".toList, Token.BracketStart),
                                             LiteralMatcher(")".toList, Token.BracketEnd),
                                             NumberMatcher)

    enum ParseError extends Error:
        case NoMatch


class TokenReader(private val rawReader: Iterator[Char]) extends Iterable[Either[ParseError, Token]]:
    private val tokens: LazyList[Either[ParseError, Token]] =
        LazyList.unfold(LazyList.from(rawReader)):
            case LazyList() => None
            case buffer     => Some:
                val result = Matcher.candidates.iterator.map(_(buffer))
                                    .collectFirst { case Some(value) => value }
                result match
                    case None                      => (Left(ParseError.NoMatch), LazyList.empty)
                    case Some(token, nextLazyList) => (Right(token), nextLazyList)

    lazy val parsed: Either[ParseError, List[Token]] =
        tokens.foldRight(Right(Nil): Either[ParseError, List[Token]]):
            case (_, Left(e))                  => Left(e)
            case (Left(e), _)                  => Left(e)
            case (Right(token), Right(buffer)) => Right(token :: buffer)

    override def iterator: Iterator[Either[ParseError, Token]] = tokens.iterator
