package com.goral09.fp_in_scala.chapter5.LazyList

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
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
}