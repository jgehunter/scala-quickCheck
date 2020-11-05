package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertMin") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == math.min(n1, n2)
  }

  property("deleteEmpty") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("ordered") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
      isSorted(h)
  }

  property("minimumMeld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  property("empty") = forAll { (a: A) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("empty2") = forAll { (a:A, b: A) =>
    val h = insert(a, insert(b, empty))
    isEmpty(deleteMin(deleteMin(h)))
  }

  property("meldAndEmpty") = forAll { (h: H) =>
    meld(h, empty) == h
  }

  property("sortedAfterMeld") = forAll { (h1: H, h2: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(meld(h1, h2))
  }

  property("minN") = forAll { (l: List[A]) =>
    case class BH(b: Boolean, h: H)

    val h = l.foldLeft(empty) { (h: H, a: A) => insert(a, h) }

    val res = l.sorted.foldLeft(BH(true, h)) {
      (bh: BH, a: A) => BH(bh.b && findMin(bh.h) == a, deleteMin(bh.h))
    }
    res.b && isEmpty(res.h)
  }

}
