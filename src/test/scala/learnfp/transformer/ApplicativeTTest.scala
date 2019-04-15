package learnfp.transformer

import learnfp.transformer.IdT._
import org.scalatest.{Matchers, WordSpecLike}

import learnfp.functor.Functor
import learnfp.functor.FunctorOps._
import learnfp.monad.Monad
import learnfp.monad.MonadOps.toMonadOpsPure

import learnfp.functor.Id
import learnfp.functor.IdInstance._
import learnfp.monad.IdInstance._

import learnfp.functor.Maybe._
import learnfp.functor.MaybeInstance._
import learnfp.monad.MaybeInstance._

import learnfp.functor.ListInstance._
import learnfp.monad.ListInstance._

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._
import learnfp.monad.DisjunctionInstance._

import learnfp.transformer.MonadTransformer._

class ApplicativeTTest extends WordSpecLike with Matchers {
  "ApplicativeT" should {

    def testPure[M[_]](implicit functor:Functor[M], monad:Monad[M]) = {
      type App[A] = IdT[A, M];
      {
        for {
          x <- 10.pure[App]
          y <- 20.pure[App]
          z <- 30.pure[App]
        } yield { x + y + z }
      }.runIdT shouldBe (10+20+30).pure[App].runIdT
    }

    def testLift[M[_]](implicit f:Functor[M], m:Monad[M]) = {
      IdT.idtMonadTransInstance[M].lift(m.pure(10)).runIdT shouldBe IdT.idtMonadInstance[M].pure(10).runIdT
    }
  }
}

