package com.goral09.fp_in_scala.chapter4.Either

sealed trait Either[+Err, +V] {
  def map[B](f: V => B): Either[Err, B] = this match {
    case Right(value) => Right(f(value))
    case Left(err) => Left(err)
  }
  def flatMap[EE >: Err, B](f: V => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(err) => Left(err)
  }
  def orElse[EE >: Err,B >: V](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => Right(value)
    case Left(_) => b 
  }
  def map2[EE >: Err, B, C](b: Either[EE, B])(f: (V, B) => C): Either[EE, C] = for {
    myRight <- this
    otRight <- b
  } yield f(myRight, otRight)
}

case class Left[+Err](err: Err) extends Either[Err, Nothing] 
case class Right[+V](value: V)  extends Either[Nothing, V] 

object Either {
  def sequence[E, V](l: List[Either[E, V]]): Either[E, List[V]] = 
    traverse(l)(x => x)

  def sequence2[E,V](l: List[Either[E,V]]): Either[E, List[V]] = 
    l.foldRight[Either[E,List[V]]](Right(Nil:List[V]))((curr, acc) => curr.map2(acc)(_::_))

  def traverse[E, V, C](l: List[Either[E,V]])(f: Either[E,V] => Either[E, C]): Either[E, List[C]] = 
    l.foldRight[Either[E, List[C]]](Right(Nil:List[C]))((curr, acc) => f(curr).map2(acc)(_ :: _))
}