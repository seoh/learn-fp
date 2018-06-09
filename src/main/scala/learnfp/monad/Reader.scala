package learnfp.monad

import learnfp.functor.Reader

object ReaderInstance {
  import learnfp.functor.ReaderInstance._

  implicit def monadReaderInstance[E] = new Monad[({type ReaderE[A] = Reader[E, A]})#ReaderE] {
    override def pure[A](a: A): Reader[E, A] = Reader(_ => a)
    override def flatMap[A, B](a: Reader[E, A])(fx: A => Reader[E, B]): Reader[E, B] = {
      Reader { e =>
        fx(a.run(e)).run(e)
      }
    }
  }
}
