package com.goral09.fp_in_scala.chapter4.Errors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(el) => Some(f(el))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(el) => el
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map(f) getOrElse None

  
  def orElse[B >: A](or: => Option[B]): Option[B] = this map(Some(_)) getOrElse or


  def filter(predicate: A => Boolean) = this flatMap { a => 
    if(predicate(a)) Some(a) else None
  }

   def map2[B,C](b: Option[B])(f: (A,B) => C): Option[C] = for {
    fa <- this
    fb <- b
  } yield f(fa,fb)
}

case class Some[+A](el: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def sequence[A](l: List[Option[A]]): Option[List[A]] = 
    l.foldRight[Option[List[A]]](Some(Nil:List[A]))((curr,acc) => curr.map2(acc)(_ :: _))

  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = 
    l.foldRight[Option[List[B]]](Some(Nil:List[B]))((curr, acc) => f(curr).map2(acc)(_::_))

  def sequence2[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(x => x)
  // Fun with Options to be continued
}