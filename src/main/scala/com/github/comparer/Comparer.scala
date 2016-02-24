package com.github.comparer

import java.io.File

import scala.io.Source

/**
  * Created by stremlenye on 24/02/16.
  */
object Comparer {

  type Line = Option[(String, Int)]

  def compare(path1: String, path2: String): Seq[(Line, Line)] = {
    compareLines(Source.fromFile(path1).getLines(), Source.fromFile(path2).getLines()).toSeq
  }

  def compareLines(linesSource1: Iterator[String], linesSource2: Iterator[String]): Iterator[(Line, Line)] = {

    class LinesLike(iterator: Iterator[String]) {
      def toLines: Iterator[Line] = iterator.zip(Stream.from(1).toIterator).map(Some(_))
    }

    implicit def toLinesLike(iterator: Iterator[String]): LinesLike = new LinesLike(iterator)

    linesSource1.toLines.zipAll(linesSource2.toLines, None, None).filter({
      case (Some(line1), Some(line2)) if line1 == line2 => false
      case _ => true
    })
  }
}
