package com.github.comparer

import scala.language.implicitConversions

abstract class Line[A](row: Int) {
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
  override def isEmpty: Boolean = false

  override def get: A = text
}

case class EmptyLine[A](row: Int) extends Line[A](row) {
  override def isEmpty: Boolean = true

  override def get: A = throw new NoSuchElementException("EmptyLine.get")
}

case class MatchLine[A](row: Int, left: Line[A], right: Line[A])

object TextComparer {
  def compareText[A](left: List[A], right: List[A]): List[MatchLine[A]] = {
    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Option[A], r: Option[A]), row) => (row, Line(row, l), Line(row, r))
    }).flatMap {
      case (row, l: TextLine[A], r: TextLine[A]) if l.text == r.text => List(MatchLine(row, l, r))
      case (row, l: TextLine[A], r: TextLine[A]) if l.text != r.text =>
        List(MatchLine(row, l, EmptyLine(row)), MatchLine(row, EmptyLine(row), r))
      case (row, l, r) => List(MatchLine(row, l, r))
    }
  }
}
