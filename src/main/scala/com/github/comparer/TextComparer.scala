package com.github.comparer

import scala.language.implicitConversions

trait Comparable[A] {
  def compare(l: A, r: A): Boolean
}

object Comparable {
  implicit def numericIsComparable[A](implicit numeric: Numeric[A]): Comparable[A] = new Comparable[A] {
    override def compare(l: A, r: A): Boolean = numeric.equiv(l,r)
  }
  implicit val stringIsComparable = new Comparable[String] {
    override def compare(l: String, r: String): Boolean = l == r
  }
}

trait WithChildren[A] {
  def children(a: A): Seq[A]
}

object WithChildren {
  implicit def withoutChildren[A]: WithChildren[A] = new WithChildren[A] {
    override def children(a: A): Seq[A] = Seq.empty[A]
  }
}

sealed trait Match[A] {
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
  def compare[A](left: Seq[A], right: Seq[A])
                (implicit comparable: Comparable[A], c: WithChildren[A]): Seq[Match[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Some[A], r: Some[A]), row) if comparable.compare(l.get, r.get) =>
        Equal(row, l, r, Comparer.compare(c.children(l.get), c.children(r.get)))
      case ((None, None), row) => Equal[A](row, None, None, Seq.empty)
      case ((l,r), row) =>
        Diff(row, l, r, Comparer.compare(l.map(c.children).getOrElse(Seq.empty[A]), r.map(c.children).getOrElse(Seq.empty[A])))
    })
  }
}