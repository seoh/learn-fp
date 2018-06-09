package learnfp.applicative

import org.scalatest.{Matchers, WordSpecLike}

import learnfp.functor.Reader
import learnfp.functor.FunctorOps._
import learnfp.functor.ReaderInstance._
import learnfp.applicative.ApplicativeOps._
import learnfp.applicative.ReaderInstance._

class ReaderTest extends WordSpecLike with Matchers {
  "reader applicative" should {
    "work" in {
      val show = Reader[Int, String](x => x.toString)
      val repeat = Reader[Int, String => String](int => str => str * int)

      (repeat <*> show).run(3) shouldBe "333"
    }
  }
}

