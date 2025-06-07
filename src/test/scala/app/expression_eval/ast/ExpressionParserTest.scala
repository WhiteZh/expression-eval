package app.expression_eval.ast

import app.expression_eval.token.Token
import org.scalatest.funsuite.AnyFunSuiteLike

object ExpressionParserTest:
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

class ExpressionParserTest extends AnyFunSuiteLike:
    import ExpressionParserTest.*
    import ExpressionParserTest.given

    test("addition"):
        val tokens         = List[Token](2.0, P, 3.0)
        val expectedResult = 5.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("substraction"):
        val tokens         = List[Token](2.0, M, 3.0)
        val expectedResult = -1.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("multiplication"):
        val tokens         = List[Token](2.0, T, 3.0)
        val expectedResult = 6.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("division"):
        val tokens         = List[Token](2.0, D, 4.0)
        val expectedResult = 0.5
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("bracket"):
        val tokens         = List[Token](Bs, 5, Be)
        val expectedResult = 5.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("chained operation"):
        val tokens         = List[Token](5, P, 5, P, 5)
        val expectedResult = 15.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("weighted chained operation"):
        val tokens         = List[Token](5, P, 5, D, 5)
        val expectedResult = 6.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("two brackets"):
        val tokens         = List[Token](Bs, 5, Be, P, Bs, 5, Be)
        val expectedResult = 10.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("two brackets advanced"):
        val tokens         = List[Token](Bs, 1, P, 2, Be, T, 3)
        val expectedResult = 9.0
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("general test 1"):
        val tokens         = List[Token](Bs, 1, P, 2, Be, T, 3, D, 4, M, 1)
        val expectedResult = 1.25
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))

    test("general test 2"):
        val tokens         = List[Token](Bs, 4, D, 3, Be, T, Bs, 5, P, Bs, 6, M, 7, Be, M, Bs, Bs, 6, P, Bs, 7, Be, T, 2, Be, Be, Be)
        val expectedResult = (4.0 / 3.0) * (5 + (6 - 7) - ((6 + (7) * 2)))
        val actualResult   = ExpressionParser.parse(tokens)

        assert(Right(expectedResult) == actualResult.map(_.evaluate))
