package learnfp.monad

import learnfp.functor.Reader
import org.scalatest.{Matchers, WordSpecLike}
import learnfp.monad.ReaderInstance._
import learnfp.functor.ReaderInstance._
import learnfp.monad.MonadOps._

class ReaderTest extends WordSpecLike with Matchers {
  "reader monad" should {
    "work" in {
      implicit val intInstance = monadReaderInstance[Int]
      type IntReader[A] = Reader[Int, A]

      (1.pure[IntReader] >>= { x: Int => x.toString.pure[IntReader] }).run(1) shouldBe 1
    }
  }
}
