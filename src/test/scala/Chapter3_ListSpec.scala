package com.goral09.fp_in_scala.chapter3
package com.goral09.fp_in_scala.test

import org.specs2.mutable._

class ListSpec extends Specification {
  "List" should {
    "return proper sum of" in {
      "non-empty list" in {
        val l = Cons(10, Cons(11, Cons(2, Nil)))

        List.sum(l) === 23
      }      
    }  

    "return proper tail for given" in {
      "non-empty list" in {
      val l = Cons(10, Cons(11, Cons(2, Nil)))

      List.tail(l) === Cons(11, Cons(2, Nil))
      }  

      "return empty tail if given list was empty" in {
        List.tail(Nil) === Nil
      }
    }

    "return correct values for given number of elements to drop" in {
      "non-empty list" in {
        val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

        List.drop(3, l) === Cons(4, Nil)
      }

      "empty list" in {
        List.drop(4, Nil) === Nil
      }
    }

    "drop values while they satisify predicate" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.dropWhile(l)(_ < 3) === Cons(3, Cons(4, Nil))
    } 

    "replace head value" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))


      List.setHead(l)(10) === Cons(10, Cons(2, Cons(3, Cons(4, Nil))))
    }

    "return all but last elements" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.init(l) === Cons(1, Cons(2, Cons(3, Nil)))
    }
  }
}
