package app

package object expression_eval:
    def unreachable: Nothing = throw new AssertionError("The unreachable is reached!")

    type NumericType = Double
