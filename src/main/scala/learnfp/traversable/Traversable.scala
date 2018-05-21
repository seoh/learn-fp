package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.foldable.Foldable
import learnfp.functor.{Functor, Id}


trait Traversable[C[_]] {
  def traverse[A, B, F[_]](xs:C[F[A]])(fx:A => B)(implicit foldable: Foldable[C], functor:Functor[F], applicative: Applicative[F]):F[C[B]]
}

object TraversableInstances {
  import learnfp.foldable.FoldableInstances._
  import learnfp.applicative.ApplicativeOps._
  import learnfp.functor.FunctorOps._

  implicit val idTraversableInstance = new Traversable[Id] {
    override def traverse[A, B, F[_]](xs: Id[F[A]])(fx:A => B)(implicit foldable: Foldable[Id], functor:Functor[F], applicative: Applicative[F]): F[Id[B]] =
      xs.value.map(a => Id(fx(a)))
  }

  type STuple3[A] = (A, A, A)
  def stuple3[A](a:A, b:A, c:A):STuple3[A] = (a, b, c)

  implicit val tuple3TraversableInstance = new Traversable[STuple3] {
    override def traverse[A, B, F[_]](xs: (F[A], F[A], F[A]))(fx: A => B)(implicit foldable: Foldable[STuple3],
                                                                          functor: Functor[F], applicative: Applicative[F]): F[(B, B, B)] = {
      foldable.foldr(xs)(applicative.pure(List.empty[B])) { (fa, acc) =>
        fa.fmap(a => (bs: List[B]) => fx(a) :: bs) <*> acc
      } fmap {
        case (b1 :: b2 :: b3 :: _) => (b1, b2, b3)
        case _ => throw new Exception
      }

/**
      val (fa1, fa2, fa3) = xs
      val fb1 = fa1 fmap fx
      val fb2 = fa2 fmap fx
      val fb3 = fa3 fmap fx

      val fb12: F[(B, B)] =
        fb1.fmap(b1 => (b2: B) => (b1, b2)) <*> fb2
      val fb123: F[(B, B, B)] =
        fb12.fmap(b12 => (b3: B) => (b12._1, b12._2, b3)) <*> fb3

      fb123
**/
    }

  }

  implicit val listTraversableInstance = new Traversable[List] {
    override def traverse[A, B, F[_]](xs: List[F[A]])(fx: A => B)(implicit foldable: Foldable[List],
                                                                  functor: Functor[F], applicative: Applicative[F]): F[List[B]] =
    xs.foldRight(applicative.pure(List.empty[B])) { (fa, acc) =>
      fa.fmap(a => (bs: List[B]) => fx(a) :: bs) <*> acc
    }
  }
}

class TraversableOps[A, C[_], F[_]](xs:C[F[A]])(implicit foldable: Foldable[C], traversable: Traversable[C],
                                                functor: Functor[F], applicative: Applicative[F]) {
  def traverse[B](fx: A => B): F[C[B]] = traversable.traverse(xs)(fx)
  def sequence: F[C[A]] = traversable.traverse(xs)(a => a)
}

object TraversableOps {
  implicit def toTraversableOps[A, C[_], F[_]](xs:C[F[A]])(implicit functor:Functor[F], applicative: Applicative[F],
                                                           foldable:Foldable[C], traversable: Traversable[C]) = new TraversableOps[A, C, F](xs)
}

