package com.goral09.fp_in_scala.chapter5.LazyList

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case Some((hd, tl)) => hd :: tl.toList
    case None => Nil
  }

  def toList_tailrec: List[A] = {
    @annotation.tailrec
    def go(tmp: List[A], s: Stream[A]): List[A] = s.uncons match {
      case Some((hd,tl)) => go(hd :: tmp, tl)
      case None => tmp
    }

    go(Nil:List[A], this).reverse
  }

  def take(n: Int): Stream[A] = {
    if(n > 0) uncons match{
      case Some((hd, tl)) if(n == 1) => Stream.cons(hd, Stream.empty[A])
      case Some((hd,tl)) => Stream.cons(hd, tl.take(n-1))
      case None => Stream.empty[A]
    } else Stream.empty[A]
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((hd, tl)) if(p(hd)) => Stream.cons(hd, tl.takeWhile(p))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some((hd, tl)) => f(hd, tl.foldRight(z)(f))
    case None => z
  }

  def exists(pred: A => Boolean): Boolean = foldRight(false)((c,a) => pred(c) || a)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((c,a) => p(c) && a)

  def takeWhile_2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (c,a) =>
    if(p(c)) Stream.cons(c,a )
    else Stream.empty
  }

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]){ (c,a) => Stream.cons(f(c), a) }
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]){(c,a) => f(c).append(a)}
  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]){(c,a) => if(f(c)) Stream.cons(c,a) else a }
  def append[A1 >: A](other: Stream[A1]): Stream[A1] = foldRight(other){(c,a) => Stream.cons[A1](c,a)}
}


object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons = Some((hd, tl))
  }

  def apply[A](els: A*): Stream[A] = 
    if(els.isEmpty) empty
    else cons(els.head, apply(els.tail: _*))

  def constant[A](el: A): Stream[A] = cons(el, constant(el))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = ???
}