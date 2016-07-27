package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.Nil

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val oneOrMore: Gen[H] = for {
    key <- arbitrary[Int]
    m <- oneOf(const(QuickCheckHeap.this.empty), oneOrMore)
  } yield insert(key, m)
  lazy val genHeap = oneOf(const(empty), oneOrMore)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Minimum of insert(a,b) into empty returns min(a,b)") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("add a to empty heap, delete minimum, result is empty heap") = forAll { a: Int =>
    val m = insert(a, empty)
    isEmpty(deleteMin(m))
  }

  property("Given any heap, continually finding and deleting minima gives a sorted sequence") = forAll { h: H =>
    def delMinANDConcat(h: H, l: List[Int]): List[Int] = {
      if (isEmpty(h)) l
      else delMinANDConcat(deleteMin(h), findMin(h) :: l)
    }

    def isSorted(l: List[Int]): Boolean = l match {
      case Nil => true
      case x :: Nil => true
      case x :: xs => if (x <= xs.head) isSorted(l.tail) else false
    }
    val supposedlySortedSequence = delMinANDConcat(h, List())
    isSorted(supposedlySortedSequence.reverse)
  }

  property("findMin(meld(a,b)) returns findMin(a) or findMin(b)") = forAll { (h1: H, h2: H) =>
    (!(isEmpty(h1) || isEmpty(h2))) ==> {
      val minOfMeld = findMin(meld(h1, h2))
      findMin(h1) == minOfMeld || findMin(h2) == minOfMeld
    }
  }

  property("min after insert element gt min of a random heap should yield min") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) Int.MinValue else a
    val h1 = insert(b + 1, insert(b, insert(b + 2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(b + 2, insert(b + 1, empty))
  }

}
