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

    "properly count length of list" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.length(l) === 4
    }

    "properly sum of elements of the list using foldRight" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.sum2(l) === 10
    }

    "properly count product of elements of the list using foldRight" in {
      val l = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))

      List.product2(l) === 24
    }

    "foldLeft should properly count sum of elements" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))


      List.sum3(l) === 10
    }

    "foldLeft should properly count product of elements" in {
      val l = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))

      List.product3(l) === 24
    }

    "foldLeft should properly compute length of the list" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.length2(l) === 4
    }

    "list should be properly reverted" in {
      "using foldLeft" in {
        val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

        List.reverse2(l) === Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
      }

      "without using fold" in {
        val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

        List.reverse(l) === Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
      }
    }

    "foldLeft implemented using foldRight" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      pending
      
      List.foldLeftViaFoldRight(List.apply[Int]())(l)((acc, curr) => Cons(curr,acc)) === List.reverse(l)
    }

    "foldRight implemented using foldLeft" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      List.foldRightViaFoldLeft(1.0)(l)(_ * _) === 24
    }

    "append using foldRight" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      val el = Cons(5, Nil)

      List.appendViaFoldRight(l,el) === Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    }

    "concat using foldRight" in {
      val l = Cons(Cons(11, Cons(12, Cons(13, Nil))), Nil)

      List.concat(l) === Cons(11, Cons(12, Cons(13, Nil)))
    }

    "map using foldRight" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      val f: Int => Int = _ + 10

      List.map(l)(f) === Cons(11, Cons(12, Cons(13, Cons(14, Nil))))
    }

    "filter using foldRight" in {
      val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

      val f: Int => Boolean = _ < 3

      List.filter(l)(f) === Cons(1, Cons(2, Nil))
    }

    "flatMap using foldRight" in {
      pending
    }

    "flatMap without foldRight" in {
      pending
    }

    "filter via flatMap" in {
      pending
    }
  }
}
