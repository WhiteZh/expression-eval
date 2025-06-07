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

    test("test1"):
        val tokenReader    = TokenReader("  ( 1+2) *3 /  4 - 1  ".iterator)
        val parsedTokens   = tokenReader.parsed
        val supposedTokens = List[Token](Bs, 1, P, 2, Be, T, 3, D, 4, M, 1)

        assert(parsedTokens == Right(supposedTokens))

    test("test2"):
        val tokenReader    = TokenReader("(4 / 3) * (5 + (6 - 7) - ((6 + (7) * 2)))".iterator)
        val parsedTokens   = tokenReader.parsed
        val supposedTokens = List[Token](Bs, 4, D, 3, Be, T, Bs, 5, P, Bs, 6, M, 7, Be, M, Bs, Bs, 6, P, Bs, 7, Be, T, 2, Be, Be, Be)

        assert(parsedTokens == Right(supposedTokens))
