package learnfp.applicative

import learnfp.functor.{Reader, ReaderInstance => ReaderFunctorInstance}

object ReaderInstance {
  import ReaderFunctorInstance._

  implicit def readerApplicativeInstance[E] = new Applicative[
    ({type ReaderE[A] = Reader[E, A]})#ReaderE
  ] {
    override def pure[A](a: A): Reader[E, A] = Reader(_ => a)
    override def <*>[A, R](fx: Reader[E, A => R])(a: Reader[E, A]): Reader[E, R] =
      Reader { e =>
        val f = fx.run(e)
        val aa = a.run(e)

        f(aa)
      }
  }
}
