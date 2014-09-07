package com.goral09.fp_in_scala.chapter3


sealed trait List[+A]
case class Cons[+A](h: A, t: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
   def sum(l: List[Int]): Int= l match {
    case Nil => 0
    case Cons(h, tail) => h + sum(tail)
  }

  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, tail) => h * product(tail)
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
  def product2(l: List[Dobule]) = foldRight(1.0)(l)(_*_)
}