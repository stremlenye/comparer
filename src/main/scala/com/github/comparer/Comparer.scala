package com.github.comparer

import scala.language.implicitConversions

/**
  * Created by stremlenye on 24/02/16.
  */
object Comparer {

  abstract class Line[A](row: Int) extends TraversableOnce[Line[A]]{
    self =>
    def isEmpty: Boolean
    def isDefined: Boolean = !isEmpty
    override def hasDefiniteSize: Boolean = true
    override def foreach[U](f: (Line[A]) => U): Unit = f(self)
    override def seq: TraversableOnce[Line[A]] = self
    override def copyToArray[B >: Line[A]](xs: Array[B], start: Int, len: Int): Unit = Array(self, xs)
    override def forall(p: (Line[A]) => Boolean): Boolean = p(self)
    override def find(p: (Line[A]) => Boolean): Option[Line[A]] = if(isDefined) Some(self) else None
    override def exists(p: (Line[A]) => Boolean): Boolean = p(self)
    override def toTraversable: Traversable[Line[A]] = new Traversable[Line[A]] {
      override def foreach[U](f: (Line[A]) => U): Unit = f(self)
    }
    override def toStream: Stream[Line[A]] = Stream(self)
    override def toIterator: Iterator[Line[A]] = Iterator(self)
    override def isTraversableAgain: Boolean = true

  }

  case class TextLine[A](row: Int, text: A) extends Line[A](row) {
    override def isEmpty: Boolean = false
  }

  case class EmptyLine[A](row: Int) extends Line[A](row) {
    override def isEmpty: Boolean = true
  }

  case class MatchLine[A](row: Int, left: Line[A], right: Line[A])

  def compareText[A](left: List[A], right: List[A]): List[MatchLine[A]] = {

    def toLine[T](opt: Option[T], row: Int): Line[T] = opt.map(TextLine(row, _)).getOrElse(EmptyLine(row))

    left.map(Some(_)).zipAll(right.map(Some(_)), None, None).zipWithIndex.map({
      case ((l: Option[A], r: Option[A]), row) => (row, toLine(l, row), toLine(r, row))
    }).flatMap {
      case (row, l: TextLine[A], r: TextLine[A]) if l.text == r.text => List(MatchLine(row, l, r))
      case (row, l: TextLine[A], r: TextLine[A]) if l.text != r.text =>
        List(MatchLine(row, l, EmptyLine(row)), MatchLine(row, EmptyLine(row), r))
      case (row, l, r) => List(MatchLine(row, l, r))
    }
  }
}
