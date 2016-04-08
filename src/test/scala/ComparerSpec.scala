import scala.math.Numeric._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

import com.github.comparer._
import com.github.comparer.Comparable._
import com.github.comparer.WithChildren._

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
    val result: Seq[Match[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      Match(0, Equal(Some(1), Some(1)), Seq.empty[Match[Int]]),
      Match(1, Equal(Some(2), Some(2)), Seq.empty[Match[Int]]))
  }

  def diffTextWithEqualLength = {
    val text1 = List(1,2)
    val text2 = List(2,1)
    val result: Seq[Match[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      Match(0, Diff(Some(1), Some(2)), Seq.empty[Match[Int]]),
      Match(1, Diff(Some(2), Some(1)), Seq.empty[Match[Int]])
    )
  }

  def diffTextWithDifferentLength = {
    val text1 = List(1,2,3)
    val text2 = List(1,2)
    val result: Seq[Match[Int]] = Comparer.compare(text1, text2)
    result must be equalTo Seq(
      Match(0, Equal(Some(1), Some(1)), Seq.empty[Match[Int]]),
      Match(1, Equal(Some(2), Some(2)), Seq.empty[Match[Int]]),
      Match(2, Diff(Some(3), None), Seq.empty[Match[Int]])
    )
  }

  def equalNested = {
    val left = List(1,2)
    val right = List(1,2)
    implicit val withChildren: WithChildren[Int] = new WithChildren[Int] {
      override def children(a: Int): Seq[Int] = if(a == 1) List(3,4) else Seq.empty[Int]
    }
    val result: Seq[Match[Int]] = Comparer.compare(left, right)
    result must be equalTo Seq(
      Match(0, Equal(Some(1), Some(1)),
        Seq(Match(0, Equal(Some(3), Some(3)), Seq.empty[Match[Int]]), Match(1, Equal(Some(4), Some(4)), Seq.empty[Match[Int]]))),
      Match(1, Equal(Some(2), Some(2)), Seq.empty[Match[Int]]))
  }

  def diffNested = {
    val left = List(1,2,3)
    val right = List(1,2)
    val children1 = List(3,4)
    val children2 = List(3,5)
    var switch = 0
    implicit val withChildren: WithChildren[Int] = new WithChildren[Int] {
      override def children(a: Int): Seq[Int] = if(a == 2) {
        switch += 1
        if(switch == 1) children1 else children2
      } else Seq.empty[Int]
    }
    val result: Seq[Match[Int]] = Comparer.compare(left, right)
    result must be equalTo Seq(
      Match(0, Equal(Some(1), Some(1)), Seq.empty[Match[Int]]),
      Match(1, Equal(Some(2), Some(2)),
        Seq(Match(0, Equal(Some(3), Some(3)), Seq.empty[Match[Int]]), Match(1, Diff(Some(4), Some(5)), Seq.empty[Match[Int]]))),
      Match(2, Diff(Some(3), None), Seq.empty[Match[Int]])
    )
  }
}
