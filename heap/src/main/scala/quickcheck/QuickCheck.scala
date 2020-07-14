package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      i <- arbitrary[Int]
      h <- oneOf(Gen.const(empty), genHeap)
    } yield insert(i, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, 
  // finding the minimum of the resulting heap should get the 
  // smallest of the two elements back.
  property("insertion") = forAll { (a: Int, b: Int) =>
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      findMin(h2) == (a min b)
  }

  // If you insert an element into an empty heap, then delete 
  // the minimum, the resulting heap should be empty.
  property("delete") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2) == true 
  }

  // Given any heap, you should get a sorted sequence of elements 
  // when continually finding and deleting minima. (Hint: recursion 
  // and helper functions are your friends.)
  // property("sorted sequence of elements") 
  property("sequence") = forAll { (h: H) =>
    def isSorted(h1:H): Boolean = 
      if(isEmpty(h1)) true
      else {
        val m = findMin(h1)
        val h2 = deleteMin(h1)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  // Finding a minimum of the melding of any two heaps should 
  // return a minimum of one or the other.
  property("meld") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min3 = findMin(meld(h1, h2))
    min3 == min1 || min3 == min2
  }

  property("heap equal") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    }
    val h_combined = meld(h1, h2)
    val h_transferred = meld(deleteMin(h1), insert(findMin(h1), h2))
    heapEqual(h_combined, h_transferred),
  }

}
