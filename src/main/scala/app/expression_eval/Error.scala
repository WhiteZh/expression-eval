package app.expression_eval

trait Error:
    override def toString: String = s"Error: $this"