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

  implicit def optionIsComparable[T, A <: Option[T]](implicit comparable: Comparable[T]): Comparable[A] = new Comparable[A] {
    override def compare(l: A, r: A): Boolean = (l, r) match {
      case (Some(left),Some(right)) => comparable.compare(left, right)
      case (None, None) => true
      case _ => false
    }
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
                    children: Seq[Match[A]]) extends Match[A] {
  override def isEqual:Boolean = true
}

case class Diff[A](row: Int,
                   left: Option[A],
                   right: Option[A],
                   children: Seq[Match[A]]) extends Match[A] {
  override def isEqual: Boolean = false
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])
                (implicit comp: Comparable[A], ch: WithChildren[A]): Seq[Match[A]] = {

    def getChildren[T](o: Option[T])(implicit ch: WithChildren[T]) = o.map(ch.children).getOrElse(Seq.empty[T])

    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l, r), row) if implicitly[Comparable[Option[A]]].compare(l, r) =>
        Equal(row, l, r, Comparer.compare(getChildren(l), getChildren(r)))
      case ((l,r), row) => Diff(row, l, r, Comparer.compare(getChildren(l), getChildren(r)))
    })
  }
}