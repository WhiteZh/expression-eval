package app

package object expression_eval:
    
    trait Error:
        override def toString: String = s"Error: $this"

    type NumericType = Double
