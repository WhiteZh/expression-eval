package app.expression_eval

import org.scalatest.funsuite.AnyFunSuiteLike

class AppTest extends AnyFunSuiteLike:
    test("test 1"):
        val rawInput       = "  ( 1+2) *3 /  4 - 1  "
        val expectedResult = Right(1.25)
        val actualResult   = App.evaluate(rawInput.iterator)

        assert(expectedResult == actualResult)

    test("test 2"):
        val rawInput       = "(4 / 3) * (5 + (6 - 7) - ((6 + (7) * 2)))"
        val expectedResult = Right((4.0 / 3.0) * (5 + (6 - 7) - ((6 + (7) * 2))))
        val actualResult   = App.evaluate(rawInput.iterator)

        assert(expectedResult == actualResult)
