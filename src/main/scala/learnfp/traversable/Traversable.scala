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
    // simplist solution
    ((a1: A) => (a2: A) => (a3: A) =>
        (fx(a1), fx(a2), fx(a3))) `<$>` xs._1 `<*>` xs._2 <*> xs._3

/**
      // Aggregation on List and make it tupled by hand
      // could be generic solution with shapeless.
      foldable.foldr(xs)(applicative.pure(List.empty[B])) { (fa, acc) =>
        fa.fmap(a => (bs: List[B]) => fx(a) :: bs) <*> acc
      } fmap {
        case (b1 :: b2 :: b3 :: _) => (b1, b2, b3)
        case _ => throw new Exception
      }
**/

    }

  }

  implicit val listTraversableInstance = new Traversable[List] {
    override def traverse[A, B, F[_]](xs: List[F[A]])(fx: A => B)(implicit foldable: Foldable[List],
                                                                  functor: Functor[F], applicative: Applicative[F]): F[List[B]] = xs.foldRight(applicative.pure(List.empty[B])) { (fa, acc) =>
      fa.fmap(a => (bs: List[B]) => fx(a) :: bs) <*> acc
    }

/**
    foldable.foldr(xs)(applicative.pure(List.empty[B])) { (fa, bs) =>
      ((a: A) => fx(a) :: bs) <*> acc
    }
**/
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

