import scala.math.Numeric._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

import com.github.comparer._
import com.github.comparer.Implicits._


/**
  * Created by stremlenye on 24/02/16.
  */
class ComparerSpec extends Specification {
  override def is: SpecStructure = s2"""
Comparer

  should return seq of EqualLine's if iterators are equal                           $equalText
  should return diff if seqs are equal by length but content is different           $diffTextWithEqualLength
  should return diff if seqs are different by length and content is different       $diffTextWithDifferentLength

  should return seq of EqualLine's with children for nested data structures         $equalNested
  should return diff if seqs are different by length and content is different       $diffNested
"""

  def equalText = {
    val text1 = List(1,2)
    val text2 = List(1,2)
    val result: Seq[MatchLine[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      EqualLine(0, SomeEntry(0, 1), SomeEntry(0, 1)),
      EqualLine(1, SomeEntry(1,2), SomeEntry(1,2)))
  }

  def diffTextWithEqualLength = {
    val text1 = List(1,2)
    val text2 = List(2,1)
    val result: Seq[MatchLine[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      DiffLine(0, SomeEntry(0, 1), SomeEntry(0, 2)),
      DiffLine(1, SomeEntry(1, 2), SomeEntry(1, 1))
    )
  }

  def diffTextWithDifferentLength = {
    val text1 = List(1,2,3)
    val text2 = List(1,2)
    val result: Seq[MatchLine[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      EqualLine(0, SomeEntry(0, 1), SomeEntry(0, 1)),
      EqualLine(1, SomeEntry(1, 2), SomeEntry(1, 2)),
      DiffLine(2, SomeEntry(2, 3), EmptyEntry(2))
    )
  }

  def equalNested = {
    val left = List(1,2)
    val right = List(1,2)
    val children = List(3,4)
    val result: Seq[MatchLine[Int]] = Comparer.compare(left, right, i => if(i == 1) children else Seq.empty[Int])
    result must be equalTo Seq(
      EqualLine(0, SomeEntry(0, 1), SomeEntry(0, 1),
        Seq(EqualLine(0, SomeEntry(0, 3), SomeEntry(0, 3)), EqualLine(1, SomeEntry(1,4), SomeEntry(1,4)))),
      EqualLine(1, SomeEntry(1,2), SomeEntry(1,2)))
  }

  def diffNested = {
    val left = List(1,2,3)
    val right = List(1,2)
    val children1 = List(3,4)
    val children2 = List(3,5)
    var switch = 0
    val result: Seq[MatchLine[Int]] = Comparer.compare(left, right, i => if(i == 2) {
      switch += 1
      if(switch == 1) children1 else children2
    } else Seq.empty[Int])
    result must be equalTo Seq(
      EqualLine(0, SomeEntry(0, 1), SomeEntry(0, 1)),
      EqualLine(1, SomeEntry(1, 2), SomeEntry(1, 2),
        Seq(EqualLine(0, SomeEntry(0, 3), SomeEntry(0, 3)), DiffLine(1, SomeEntry(1,4), SomeEntry(1,5)))),
      DiffLine(2, SomeEntry(2, 3), EmptyEntry(2))
    )
  }
}
