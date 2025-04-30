package app.expression_eval.token

import org.scalatest.funsuite.AnyFunSuiteLike

class TokenReaderTest extends AnyFunSuiteLike:
    private val N  = Token.Number
    private val P  = Token.Plus
    private val M  = Token.Minus
    private val T  = Token.Times
    private val D  = Token.Div
    private val Bs = Token.BracketStart
    private val Be = Token.BracketEnd

    given Conversion[Double, Token.Number] with
        override def apply(x: Double): Token.Number = N(x)

    given Conversion[Int, Token.Number] with
        override def apply(x: Int): Token.Number = N(x)

    test("testParsed"):
        val tokenReader                 = TokenReader("  ( 1+2) *3 /  4 - 1  ".iterator)
        val parsedTokens                = tokenReader.parsed
        val supposedTokens: List[Token] = List(Bs, 1, P, 2, Be, T, 3, D, 4, M, 1)

        assert(parsedTokens == Right(supposedTokens))
