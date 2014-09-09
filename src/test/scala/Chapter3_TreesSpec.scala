package com.goral09.fp_in_scala.chapter3.Trees
package com.goral09.fp_in_scala.test

import org.specs2.mutable._

class TreeSpec extends Specification {
  "Tree" should {
    "have properly implemented size" in {
      val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

      Tree.size(t) === 3
    }

    "have properly implemented maximum" in {
      val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

      Tree.maximum(t) === 4
    }

    "have properly implemented depth" in {
      val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

      Tree.depth(t) === 2
    }

    "have properly implemented map" in {
      val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

      val f: Int => String = _.toString

      Tree.map(t)(f) === Branch(Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")), Leaf("4"))
    }
  }
}