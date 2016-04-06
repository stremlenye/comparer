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

object Match {
  def apply[A](row: Int, left: Option[A], right: Option[A])(implicit compare: (A, A) => Boolean): Match[A] = (left, right) match {
    case (Some(l), Some(r)) if compare(l, r) => Equal(row, left, right)
    case (None, None) => Equal(row, left, right)
    case _ => Diff(row, left, right)
  }

  def apply[A](row: Int,
               left: Option[A],
               right: Option[A],
               childrenLeft: => Seq[A],
               childrenRight: => Seq[A])(implicit compare: (A, A) => Boolean): Match[A] =  (left, right) match {
    case (Some(l), Some(r)) if compare(l, r) => Equal(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case (None, None) => Equal(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case _ => Diff(row, left, right, Comparer.compare(childrenLeft, childrenRight))
  }
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
      case ((l, r), row) => Match(row, l, r, l.map(children).getOrElse(Seq.empty[A]), r.map(children).getOrElse(Seq.empty[A]))
    })
  }
}