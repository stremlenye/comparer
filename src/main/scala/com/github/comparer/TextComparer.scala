package com.github.comparer

import scala.language.implicitConversions
import scalaz.Functor

object LineImplicits {
  val lineIsFunctor: Functor[Entry] = new Functor[Entry] {
    override def map[A, B](fa: Entry[A])(f: (A) => B): Entry[B] = if(fa.isDefined) SomeEntry(f(fa.get)) else EmptyEntry
  }
}

sealed trait Entry[+A] {
  self =>
  def isEmpty: Boolean
  def isDefined: Boolean = !isEmpty

  def get: A
  def getOrElse[B >: A](a: => B): B = if(isDefined) self.get else a
}

object Entry {
  def apply[A](a: Option[A]): Entry[A] = a.map(e => SomeEntry(e)).getOrElse(EmptyEntry)
}

case class SomeEntry[A](text: A) extends Entry[A] {
  override def isEmpty = false

  override def get: A = text
}

case object EmptyEntry extends Entry[Nothing] {

  override def isEmpty = true

  override def get: Nothing = throw new NoSuchElementException("EmptyLine.get")
}

sealed abstract class Match[A] {
  val row: Int
  val left: Entry[A]
  val right: Entry[A]
  def isEqual: Boolean

  def children: Seq[Match[A]]
  def hasChildren: Boolean = children.nonEmpty
}

object Match {
  def apply[A](row: Int, left: Entry[A], right: Entry[A])(implicit compare: (A, A) => Boolean): Match[A] = (left, right) match {
    case (SomeEntry(l), SomeEntry(r)) if compare(l, r) => Equal(row, left, right)
    case (EmptyEntry, EmptyEntry) => Equal(row, left, right)
    case _ => Diff(row, left, right)
  }

  def apply[A](row: Int,
               left: Entry[A],
               right: Entry[A],
               childrenLeft: => Seq[A],
               childrenRight: => Seq[A])(implicit compare: (A, A) => Boolean): Match[A] =  (left, right) match {
    case (SomeEntry(l), SomeEntry(r)) if compare(l, r) => Equal(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case (EmptyEntry, EmptyEntry) => Equal(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case _ => Diff(row, left, right, Comparer.compare(childrenLeft, childrenRight))
  }
}

case class Equal[A](row: Int,
                    left: Entry[A],
                    right: Entry[A],
                    children: Seq[Match[A]] = Seq.empty[Match[A]]) extends Match[A] {
  override def isEqual:Boolean = true
}

case class Diff[A](row: Int,
                   left: Entry[A],
                   right: Entry[A],
                   children: Seq[Match[A]] = Seq.empty[Match[A]]) extends Match[A] {
  override def isEqual: Boolean = false
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[Match[A]] = compare(left, right, _ => Seq.empty[A])

  def compare[A](left: Seq[A], right: Seq[A], children: A => Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[Match[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Option[A], r: Option[A]), row) => Match(row,
        Entry(l),
        Entry(r),
        l.map(children).getOrElse(Seq.empty[A]),
        r.map(children).getOrElse(Seq.empty[A]))
    })
  }
}