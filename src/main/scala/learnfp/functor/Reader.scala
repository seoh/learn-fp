package learnfp.functor

case class Reader[E, A](run: E => A)

object ReaderInstance {
  implicit def readerInstance[E]: Functor[({type R[A] = Reader[E, A]})#R] =
    new Functor[({type R[A] = Reader[E, A]})#R] {
      override def fmap[A, B](r: Reader[E, A])(fx: A => B): Reader[E, B] = Reader {
        e => fx(r.run(e))
      }

  }
}
