package com.github.comparer

import scala.language.implicitConversions

sealed abstract class Match[A] {
  val row: Int
  val left: Option[A]
  val right: Option[A]
  def isEqual: Boolean

  def children: Seq[Match[A]]
  def hasChildren: Boolean = children.nonEmpty
}

case class Equal[A](row: Int,
                    left: Option[A],
                    right: Option[A],
                    children: Seq[Match[A]] = Seq.empty[Match[A]]) extends Match[A] {
  override def isEqual:Boolean = true
}

case class Diff[A](row: Int,
                   left: Option[A],
                   right: Option[A],
                   children: Seq[Match[A]] = Seq.empty[Match[A]]) extends Match[A] {
  override def isEqual: Boolean = false
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[Match[A]] = compare(left, right, _ => Seq.empty[A])

  def compare[A](left: Seq[A], right: Seq[A], children: A => Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[Match[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Some[A], r: Some[A]), row) if areEqual(l.get, r.get) => Equal(row, l, r, Comparer.compare(children(l.get), children(r.get)))
      case ((None, None), row) => Equal[A](row, None, None, Seq.empty)
      case ((l,r), row) => Diff(row, l, r, Comparer.compare(l.map(children).getOrElse(Seq.empty[A]), r.map(children).getOrElse(Seq.empty[A])))
    })
  }
}