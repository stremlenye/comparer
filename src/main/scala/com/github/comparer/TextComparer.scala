package com.github.comparer

import scala.language.implicitConversions

sealed abstract class Line[A](row: Int) {
  self =>
  def isEmpty: Boolean
  def isDefined: Boolean = !isEmpty

  def get: A
  def getOrElse(a: => A) = if(isDefined) self.get else a

  def map[B](f: Line[A] => B): Line[B] = if(isDefined) TextLine(row, f(self)) else EmptyLine(row)
  def flatMap[B](f: A => Line[B]): Line[B] = if (isDefined) f(self.get) else EmptyLine(row)
  def filter(f: Line[A] => Boolean): Line[A] = if(isDefined && f(self)) self else EmptyLine(row)
  def foreach[U](f: Line[A] => U): Unit = f(self)
}

object Line {
  def apply[A](row: Int, a: Option[A]): Line[A] = a.map(TextLine(row,_)).getOrElse(EmptyLine(row))
}

case class TextLine[A](row: Int, text: A) extends Line[A](row) {
  override def isEmpty = false

  override def get: A = text
}

case class EmptyLine[A](row: Int) extends Line[A](row) {
  override def isEmpty = true

  override def get: A = throw new NoSuchElementException("EmptyLine.get")
}

sealed abstract class MatchLine[A] {
  val row: Int
  val left: Line[A]
  val right: Line[A]
  def isEqual: Boolean
}

object MatchLine {
  def apply[A](row: Int, left: Line[A], right: Line[A])(implicit compare: (A, A) => Boolean): MatchLine[A] = (left, right) match {
    case (TextLine(_, l), TextLine(_, r)) if compare(l, r) => EqualLine(row, left, right)
    case (EmptyLine(_), EmptyLine(_)) => EqualLine(row, left, right)
    case _ => DiffLine(row, left, right)
  }
}

case class EqualLine[A](row: Int, left: Line[A], right: Line[A]) extends MatchLine[A] {
  override def isEqual:Boolean = true
}

case class DiffLine[A](row: Int, left: Line[A], right: Line[A]) extends MatchLine[A] {
  override def isEqual: Boolean = false
}

object Comparer {
  def compare[A](left: Seq[A], right: Seq[A])(implicit compare: (A, A) => Boolean): Seq[MatchLine[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Option[A], r: Option[A]), row) => (row, Line(row, l), Line(row, r))
    }).map { case (row, l: Line[A], r: Line[A]) => MatchLine(row, l, r) }
  }
}