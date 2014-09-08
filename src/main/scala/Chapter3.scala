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

  def ::[A](el: A, list: List[A]): List[A] = list match {
    case Nil => Cons(el, Nil)
    case l @ Cons(_,_) => Cons(el, l)
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
    while(!these.isEmpty) {
      buff = Cons(List.head(these), buff)
      these = List.tail(these)
    }
    buff
  }

  def reverse2[A](l: List[A]): List[A] = List.foldLeft(List[A]())(l)((acc,curr) => Cons(curr, acc))

  def foldLeftViaFoldRight[A, B](start: B)(l: List[A])(f: (B,A) => B): B = ???
  def foldRightViaFoldLeft[A, B](start: B)(l: List[A])(f: (A,B) => B): B = foldLeft(start)(reverse2(l))((acc, curr) => f(curr, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRightViaFoldLeft(r)(l)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = foldRight(Nil:List[A])(l)(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(Nil:List[B])(l)((curr, acc) => Cons(f(curr), acc))
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(Nil: List[A])(l)((curr, acc) => if(f(curr)) Cons(curr, acc) else acc)   

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
  def flatMap_2[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(Nil: List[B])(l)((curr, acc) => append(f(curr), acc))
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap[A,A](l)(el => if(f(el)) List(el) else Nil:List[A])
  def zipWithFun[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWithFun(t1,t2)(f))
  }
  def hasSubsequence[A](l: List[A], r: List[A]): Boolean = (l,r) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) hasSubsequence(t1,t2) else hasSubsequence(t1, r)
  }
}