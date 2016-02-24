package com.github.comparer

import scala.io.Source

/**
  * Created by stremlenye on 24/02/16.
  */
object Comparer {

  type Line[A] = Option[(A, Int)]

  def compare[A](path1: String, path2: String): Seq[(Line[String], Line[String])] = {
    compareLines[String](Source.fromFile(path1).getLines(), Source.fromFile(path2).getLines()).toSeq
  }

  def compareLines[A](linesSource1: Iterator[A], linesSource2: Iterator[A]): Iterator[(Line[A], Line[A])] = {

    class LinesLike[A](iterator: Iterator[A]) {
      def toLines: Iterator[Line[A]] = iterator.zip(Stream.from(1).toIterator).map(Some(_))
    }

    implicit def toLinesLike[A](iterator: Iterator[A]): LinesLike[A] = new LinesLike[A](iterator)

    linesSource1.toLines.zipAll(linesSource2.toLines, None, None).filter({
      case (Some(line1), Some(line2)) if line1 == line2 => false
      case _ => true
    })
  }
}
