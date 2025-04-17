package app.expression_eval

import app.expression_eval.ast.ExpressionParser
import app.expression_eval.token.TokenMatcher.{LiteralTokenMatcher, NumberTokenMatcher}
import app.expression_eval.token.{Token, TokenMatcher, TokenReader}
import lib.util.unreachable

import scala.io.Source

object App:

    def main(args: Array[String]): Unit =
        println("Program starts!")
        args match
            case Array()     => Source.fromInputStream(System.in)
                                      .getLines()
                                      .foreach(line => run(line.iterator))
            case Array(name) => run(Source.fromURL(name).iter)
            case _           => unreachable

    private def run(iter: Iterator[Char]): Unit =
        try
            evaluate(iter) match
                case Left(e)      => println(e)
                case Right(value) => println(s"=> $value")
        catch
            case e: Exception => e.printStackTrace()

    private def evaluate(iter: Iterator[Char]): Either[Error, NumericType] =
        for
            tokens <- TokenReader(iter).parsed
            node <- ExpressionParser.parse(tokens)
        yield node.evaluate
