package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    gh <- oneOf(const(empty), genHeap)
  } yield insert(a, gh)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  def recursiveMin(ts: H, as: List[Int]): List[Int] = {
    if (isEmpty(ts)) as
    else findMin(ts) :: recursiveMin(deleteMin(ts), as)
  }
  property("hint3") = forAll { (x: H) =>
    val xs = recursiveMin(x, Nil)
    xs == xs.sorted
  }
  property("hint4") = forAll { (a: H, b: H) =>
    val xa = recursiveMin(meld(a, b), Nil)
    val xb = recursiveMin(meld(deleteMin(a), insert(findMin(a), b)), Nil)
    xa == xb
  }
}
