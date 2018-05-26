package learnfp.transformer

import learnfp.functor.{Functor, FunctorOps, Id}
import learnfp.monad.{Monad, MonadOps, MonadOpsPure}

case class IdT[A, F[_]](runIdT:F[Id[A]])

object IdT {
  import FunctorOps._
  import MonadOps._

  implicit def idtFunctorInstance[F[_]](implicit outerFunctor:Functor[F]) = new Functor[({type E[X] = IdT[X, F]})#E] {
    override def fmap[A, B](a: IdT[A, F])(fx: A => B): IdT[B, F] =
      IdT(a.runIdT.fmap(ia => Id(fx(ia.value))))
  }

  implicit def idtMonadInstance[M[_]](implicit outerMonad:Monad[M], outerFunctor:Functor[M]) = new Monad[({type E[X] = IdT[X, M]})#E] {
    override def pure[A](a: A): IdT[A, M] = IdT(outerMonad.pure(Id(a)))
    override def flatMap[A, B](a: IdT[A, M])(fx: A => IdT[B, M]): IdT[B, M] =
      IdT(a.runIdT.flatMap(ia => fx(ia.value).runIdT))
  }

  implicit def idtToMonadOps[A, M[_]](a:IdT[A, M])(implicit m:Monad[M], f:Functor[M]) =
    new MonadOps[A, ({type E[X] = IdT[X, M]})#E](a)

  implicit def idtMonadTransInstance[M[_]](implicit m:Monad[M], f:Functor[M]) = new MonadTransformer[M, IdT] {
    override def lift[A](a: M[A]): IdT[A, M] = IdT(a.map(Id.apply))
  }

  def lift[A, M[_]](a:M[A])(implicit f:Functor[M], m:Monad[M]):IdT[A, M] = idtMonadTransInstance[M].lift(a)
}
