package app.expression_eval

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

    private def run(iter: Iterator[Char]): Unit = {
        try
            evaluate(iter) match
                case Left(e)      => e match
                    case e: TokenReader.ParseError       => println(s"ParseError: $e")
                    case e: ExpressionParser.SyntaxError => println(s"SyntaxError: $e")
                case Right(value) => println(s"=> $value")
        catch
            case e: Exception => e.printStackTrace()
    }

    private type Error = TokenReader.ParseError | ExpressionParser.SyntaxError

    private def evaluate(iter: Iterator[Char]): Either[Error, NumericType] =
        for
            tokens <- TokenReader(iter).parsed
            node <- ExpressionParser.parse(tokens)
        yield node.evaluate
