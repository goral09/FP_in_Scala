package com.goral09.fp_in_scala.chapter5.LazyList
package com.goral09.fp_in_scala.test

import org.specs2.mutable._

class LazyListSpec extends Specification {
  "My Stream implementation" should {
    "convert stream to List" in {
      val s = Stream(1,2,3,4,5)

      s.toList === List(1,2,3,4,5)
    }

    "convert stream to List using tailrec function" in {
      val s = Stream(1,2,3,4,5)

      s.toList_tailrec === List(1,2,3,4,5)
    }

    "take first 3 elements from stream" in {
      val s = Stream(1,2,3,4,5)

      s.take(3).toList === List(1,2,3)
    }

    "take while smaller than 4" in {
      val s = Stream(1,2,3,4,5)

      s.takeWhile(_ < 4).toList === List(1,2,3)
    }

    "foldRight properly sums" in {
      val s = Stream(1,2,3,4,5)

      s.foldRight(0){ _ + _ } === 15 
    }

    "exists implemented in terms of foldRight is lazy" in {
      val s = Stream(1,2,3,4,5)
      var ct = 0
      val isEven: Int => Boolean = _ % 2 == 0
      s.foldRight(false) { (c,a) => 
        ct += 1
        isEven(c) || a
      } === true && ct === 2
    }

    "forAll properly checks all elements in stream" in {
      val s = Stream("bob", "al", "will", "Joe")

      s.forAll { _.length <= 4 }
    } 

    "takeWhile in terms of foldRight" in {
      val s = Stream("bob", "al", "will", "Joe")

      s.takeWhile_2(_.length < 4).toList === List("bob", "al")
    }

    "map in terms of foldRight" in {
      val s = Stream("bob", "al", "will", "Joe")

      s.map(_.length).toList === List(3,2,4,3)
    }

    "append in terms of foldRight" in {
      val s1 = Stream(1,2,3)
      val s2 = Stream(4,5,6)

      s1.append(s2).toList === List(1,2,3,4,5,6)
    }

    "flatMap in terms of foldRight" in {
      val s = Stream("bob", "al", "will", "Joe")

      val f: String => Stream[Int] = w => Stream(w.length)

      s.flatMap(f).toList === s.toList.map(_.length)
    }

    "filter in terms of foldRight" in {
      val s = Stream(1,2,3,4,5,6)

      s.filter(_ % 2 == 0).toList === List(2,4,6)
    }

    "constant returns infinite stream of given elements" in {
      val s = Stream.constant(3)

      s.take(3).toList === List(3,3,3)
    }

    "from generates infinite stream of consecutive numbers" in {
      val s = Stream.from(3)

      s.take(5).toList === List(3,4,5,6,7)
    }

  }
}