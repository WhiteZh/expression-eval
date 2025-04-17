package lib

package object util:

    def unreachable: Nothing = throw new AssertionError("The unreachable is reached!")