package com.github.comparer

import scala.language.implicitConversions
import scalaz.Functor

object LineImplicits {
  val lineIsFunctor: Functor[Entry] = new Functor[Entry] {
    override def map[A, B](fa: Entry[A])(f: (A) => B): Entry[B] = if(fa.isDefined) SomeEntry(fa.row, f(fa.get)) else EmptyEntry[B](fa.row)
  }
}

sealed abstract class Entry[A] {
  self =>
  def isEmpty: Boolean
  def isDefined: Boolean = !isEmpty

  val row: Int
  def get: A
  def getOrElse(a: => A) = if(isDefined) self.get else a
}

object Entry {
  def apply[A](row: Int, a: Option[A]): Entry[A] = a.map(SomeEntry(row,_)).getOrElse(EmptyEntry(row))
}

case class SomeEntry[A](row: Int, text: A) extends Entry[A] {
  override def isEmpty = false

  override def get: A = text
}

case class EmptyEntry[A](row: Int) extends Entry[A] {
  override def isEmpty = true

  override def get: A = throw new NoSuchElementException("EmptyLine.get")
}

sealed abstract class MatchLine[A] {
  val row: Int
  val left: Entry[A]
  val right: Entry[A]
  def isEqual: Boolean

  def children: Seq[MatchLine[A]]
  def hasChildren: Boolean = children.nonEmpty
}

object MatchLine {
  def apply[A](row: Int, left: Entry[A], right: Entry[A])(implicit compare: (A, A) => Boolean): MatchLine[A] = (left, right) match {
    case (SomeEntry(_, l), SomeEntry(_, r)) if compare(l, r) => EqualLine(row, left, right)
    case (EmptyEntry(_), EmptyEntry(_)) => EqualLine(row, left, right)
    case _ => DiffLine(row, left, right)
  }

  def apply[A](row: Int,
               left: Entry[A],
               right: Entry[A],
               childrenLeft: => Seq[A],
               childrenRight: => Seq[A])(implicit compare: (A, A) => Boolean): MatchLine[A] =  (left, right) match {
    case (SomeEntry(_, l), SomeEntry(_, r)) if compare(l, r) => EqualLine(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case (EmptyEntry(_), EmptyEntry(_)) => EqualLine(row, left, right, Comparer.compare(childrenLeft, childrenRight))
    case _ => DiffLine(row, left, right, Comparer.compare(childrenLeft, childrenRight))
  }
}

case class EqualLine[A](row: Int,
                        left: Entry[A],
                        right: Entry[A],
                        children: Seq[MatchLine[A]] = Seq.empty[MatchLine[A]]) extends MatchLine[A] {
  override def isEqual:Boolean = true
}

case class DiffLine[A](row: Int,
                       left: Entry[A],
                       right: Entry[A],
                       children: Seq[MatchLine[A]] = Seq.empty[MatchLine[A]]) extends MatchLine[A] {
  override def isEqual: Boolean = false
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[MatchLine[A]] = compare(left, right, _ => Seq.empty[A])

  def compare[A](left: Seq[A], right: Seq[A], children: A => Seq[A])(implicit areEqual: (A, A) => Boolean): Seq[MatchLine[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Option[A], r: Option[A]), row) => MatchLine(row,
        Entry(row, l),
        Entry(row, r),
        l.map(children).getOrElse(Seq.empty[A]),
        r.map(children).getOrElse(Seq.empty[A]))
    })
  }
}