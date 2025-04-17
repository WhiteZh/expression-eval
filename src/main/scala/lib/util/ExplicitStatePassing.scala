package lib.util

import scala.annotation.tailrec

object ExplicitStatePassing:
    import StateLoopStatus.*

    enum StateLoopStatus[+A, +B]:
        private[ExplicitStatePassing] case ReturnResult(result: A)
        private[ExplicitStatePassing] case UpdateState(state: B)

    def returnResult[A](result: A): StateLoopStatus[A, Nothing] = ReturnResult(result)

    def updateState[B](state: B): StateLoopStatus[Nothing, B] = UpdateState(state)

    @tailrec
    def stateLoop[A, B](initialState: B)(block: B => StateLoopStatus[A, B]): A =
        block(initialState) match
            case ReturnResult(result) => result
            case UpdateState(state) => stateLoop(state)(block)
