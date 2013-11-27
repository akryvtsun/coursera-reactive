package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val min = if (a > b) b else a
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }
  
  property("del2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }    
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }
  
  property("seq") = forAll { (h: H) =>
    checkSeq(h)
  }
  
  def checkSeq(h: H): Boolean = 
    if (isEmpty(h)) true 
    else {
      val next = deleteMin(h)
      if (isEmpty(next)) true
      else (findMin(h) <= findMin(next)) && checkSeq(next)
    }
  
  property("meld1") = forAll { (h: H, h1: H) =>
    val min = findMin(h)
    val min1 = findMin(h1)
    val res = findMin(meld(h, h1))
    min == res || min1 == res
  }
  
  property("meld1.1") = forAll { (h: H, h1: H) =>
    checkSeq(meld(h,h1))
  }
  
  property("meld simmetry") = forAll { (h: H, h1: H) =>
    checkEq(meld(h,h1), meld(h1,h))
  }
  
  property("meld associativity") = forAll { (h: H, h1: H, h2: H) =>
    checkEq(meld(meld(h,h1), h2), meld(h, meld(h1,h2)))
  }
  
  def checkEq(h: H, h1: H): Boolean = 
    if (isEmpty(h) && isEmpty(h1)) true 
    else if (isEmpty(h) && !isEmpty(h1) || !isEmpty(h) && isEmpty(h1)) false
    else {
      if (findMin(h) != findMin(h1)) false
      else checkEq(deleteMin(h), deleteMin(h1))
    }
  
  property("meld2") = forAll { (h: H) =>
    findMin(meld(h, empty)) == findMin(h)
  }
  
  property("meld3") = forAll { (h: H) =>
    findMin(meld(h, h)) == findMin(h)
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    m <- oneOf(empty, genHeap)
  } yield insert(n, m)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
