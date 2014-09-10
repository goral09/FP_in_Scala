package com.goral09.fp_in_scala.chapter4.Errors

sealed trait Option[+A]
case class Some[+A](el: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map[A,B](op: Option[A])(f: A => B): Option[B] = op match {
    case Some(el) => Some(f(el))
    case None => None
  }

  def getOrElse[A, B >: A](op: Option[A])(default: => Option[B]): B = op match {
    case Some(el) => el
    case None => default
  }

  def flatMap[A,B](op: Option[A])(f: A => Option[B]): Option[B] = map(op)(f) getOrElse None

  
  def orElse[A, B >: A](op: Option[A])(or: => Option[B]): Option[B] = map(op)(Some(_)) getOrElse or


  def filter[A](op: Option[A])(predicate: A => Boolean) = flatMap(op) { a => 
    if(predicate(a)) Some(a) else None
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = for {
    fa <- a
    fb <- b
  } yield f(fa,fb)

  def sequence[A](l: List[Option[A]]): Option[List[A]] = 
    l.foldRight(Some(Nil:List[A]))((curr,acc) => map2(curr, acc)(_ :: _))

  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = 
    l.foldRight[Option[List[B]]](Some(Nil:List[B]))((curr, acc) => map2(f(curr), acc)(_::_))

  def sequence2[A](l: List[Option[A]]): Option[List[A]] = traverse(l)()


  // Fun with Options to be continued
}