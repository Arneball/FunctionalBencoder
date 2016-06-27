import scala.collection.mutable
trait Builder[Elem, +To] {
  def apply(): scala.collection.mutable.Builder[Elem, To]
}

object Builder {
  implicit def seq[T]: Builder[T, Seq[T]] = new Builder[T, Seq[T]] {
    def apply() = Seq.newBuilder[T]
  }
  implicit def setBuilder[T](): Builder[T, Set[T]] = new Builder[T, Set[T]] {
    def apply() = Set.newBuilder
  }

  def mk[T, CC[_]](cc: CC[T])(implicit buuilder: Builder[T, CC[T]]) = {
    buuilder.apply().result()
  }

  mk("")
}