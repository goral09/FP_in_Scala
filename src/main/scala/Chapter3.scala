package com.goral09.fp_in_scala.chapter3


sealed trait List[+A] { 
  def isEmpty: Boolean
}
case class Cons[+A](h: A, t: List[A]) extends List[A] {
  def isEmpty = false
}
case object Nil extends List[Nothing] {
  def isEmpty = true
}

object List {
  def apply[A](els: A*): List[A] = {
    if(els.isEmpty) Nil
    else Cons(els.head, apply(els.tail: _*))
  }
   def sum(l: List[Int]): Int= l match {
    case Nil => 0
    case Cons(h, tail) => h + sum(tail)
  }

  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, tail) => h * product(tail)
  }

  def head[A](l: List[A]): A = l match {
    case Nil => throw new NoSuchElementException("No head in empty list")
    case Cons(h,t) => h
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    def helper(w: Int, tmp: List[A]): List[A] = w match {
      case v if v == 0 => tmp
      case _ => helper(w - 1, List.tail(tmp))
    }

    helper(n, l)
  }

  def dropWhile[A](l: List[A])(predicate: A => Boolean): List[A] = {
    def helper(tmp: List[A]): List[A] = tmp match {
      case Nil => Nil
      case Cons(h, tail) if(predicate(h)) => helper(tail)
      case _ => tmp
    }

    helper(l)
  }

  def setHead[A](l: List[A])(head: A) = l match {
    case Nil => Cons(head, Nil)
    case Cons(h, t) => Cons(head, t)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](start: B)(l: List[A])(f: (A,B) => B): B = l match {
    case Nil => start
    case Cons(h, t) => f(h, foldRight(start)(t)(f))
  }

  def sum2(l: List[Int]) = foldRight(0)(l)(_+_)

  def product2(l: List[Double]) = foldRight(1.0)(l)(_*_)

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  @annotation.tailrec
  def foldLeft[A,B](start: B)(l: List[A])(f: (B,A) => B): B = l match {
    case Nil => start
    case Cons(h, t) => foldLeft(f(start, h))(t)(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(0)(l)(_ + _)
  def product3(l: List[Double]) = foldLeft(1.0)(l)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(0)(l)((acc,h) => acc + 1)

  def reverse[A](l: List[A]): List[A] =  {
    var buff: List[A] = Nil
    var these = l
    while(!l.isEmpty) {
      buff = Cons(List.head(l), buff)
      these = List.tail(these)
    }
    buff
  }
  def reverse2[A](l: List[A]): List[A] = List.foldLeft(List[A]())(l)((acc,curr) => Cons(curr, acc))

  def foldLeftViaFoldRight[A, B](start: B)(l: List[A])(f: (B,A) => B): B = ???
  def foldRightViaFoldLeft[A, B](start: B)(l: List[A])(f: (A,B) => B): B = ???

}