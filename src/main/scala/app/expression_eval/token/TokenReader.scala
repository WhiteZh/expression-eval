package app.expression_eval.token

import app.expression_eval.Error
import app.expression_eval.token.TokenReader.ParseError

object TokenReader:

    enum ParseError extends Error:
        case NoMatch

        override def toString: String = s"ParseError: $this"


class TokenReader(private val rawReader: Iterator[Char]) extends Iterable[Either[ParseError, Token]]:

    private val tokens: LazyList[Either[ParseError, Token]] =
        LazyList.unfold(LazyList.from(rawReader)):
            case LazyList() => None
            case buffer     => Some:
                val result = TokenMatcher.candidates.iterator
                                         .map(_(buffer))
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
