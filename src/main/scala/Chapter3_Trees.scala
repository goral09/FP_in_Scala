package com.goral09.fp_in_scala.chapter3.Trees

sealed trait Tree[+A]
case class Leaf[A](val a: A) extends Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum[A](t: Tree[A])(implicit m: Ordering[A]): A = t match {
    case Leaf(value) => value
    case Branch(l, r) => m.max(maximum(l), maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => depth(l) max depth(r) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}