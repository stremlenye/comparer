package com.github.comparer

import scala.language.implicitConversions

trait Comparable[A] {
  def compare(l: A, r: A): Entry[A]
}

object Comparable {
  implicit def numericIsComparable[A](implicit numeric: Numeric[A]): Comparable[A] = new Comparable[A] {
    override def compare(l: A, r: A): Entry[A] = if(numeric.equiv(l,r)) Equal(Option(l), Option(r)) else Diff(Option(l),Option(r))
  }

  implicit val stringIsComparable = new Comparable[String] {
    override def compare(l: String, r: String): Entry[String] = if(l == r) Equal(Option(l), Option(r)) else Diff(Option(l),Option(r))
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

sealed trait Entry[A] {
  val left: Option[A]
  val right: Option[A]
  def isEqual: Boolean
}

case class Equal[A](left: Option[A], right: Option[A]) extends Entry[A] {
  override def isEqual:Boolean = true
}

case class Diff[A](left: Option[A], right: Option[A]) extends Entry[A] {
  override def isEqual: Boolean = false
}

case class Match[A] (row: Int, entry: Entry[A], children: Seq[Match[A]]) {
  def hasChildren: Boolean = children.nonEmpty
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])
                (implicit comp: Comparable[A], ch: WithChildren[A]): Seq[Match[A]] = {

    def getChildren[T](o: Option[T])(implicit ch: WithChildren[T]) = o.map(ch.children).getOrElse(Seq.empty[T])

    def compareOpts[T](left: Option[T], right: Option[T])(implicit comp: Comparable[T]): Entry[T] = (left, right) match {
      case (Some(l), Some(r)) => comp.compare(l,r)
      case (None, None) => Equal(left, right)
      case _ => Diff(left, right)
    }

    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l, r), row) =>
        Match(row, compareOpts(l, r), Comparer.compare(getChildren(l), getChildren(r)))
    })
  }
}