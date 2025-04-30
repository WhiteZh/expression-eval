package app.expression_eval.token

trait SkippableMatcher extends (LazyList[Char] => Option[LazyList[Char]])

object SkippableMatcher:
    val candidates: List[SkippableMatcher] = List(SpaceSkippableMatcher)
    
    private object SpaceSkippableMatcher extends SkippableMatcher:
        override def apply(buffer: LazyList[Char]): Option[LazyList[Char]] =
            buffer match
                case LazyList() => None
                case _          => if buffer.head.isWhitespace
                                   then Some(buffer.dropWhile(_.isWhitespace))
                                   else None