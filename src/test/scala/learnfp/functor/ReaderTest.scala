package learnfp.functor

import org.scalatest.{Matchers, WordSpecLike}

class ReaderTest extends WordSpecLike with Matchers {
  import ReaderInstance._
  import FunctorOps._

  /**
  type ReaderInt[A] = Reader[Int, A]
  implicit val intReader: Functor[ReaderInt] = readerInstance[Int]
  **/
  
  "reader" should {
    val r /* : Reader[Int, String] */ = Reader[Int, String](i => i.toString)
    "work on simple functions" in {
      r.run(1) shouldBe "1"
      r fmap { str => str * 3 } run 1 shouldBe "111"
    }

    "obey identity" in {
      r.fmap(identity).run(10) shouldBe r.run(10)
    }
    
    "obey composition" in {
      val f = {x: String => x.head.toInt }
      val g = {x: Int => x - 48}

      { r fmap f fmap g }.run(1) shouldBe { r fmap (f andThen g) run 1 }
    }
  }
}
