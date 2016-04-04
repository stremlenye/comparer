import com.github.comparer._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

/**
  * Created by stremlenye on 24/02/16.
  */
class ComparerSpec extends Specification {
  override def is: SpecStructure = s2"""
Comparer

  should return empty diff if iterators are equal                                   $equalText
  should return diff if iterators are equal by length but content is different      $diffTextWithEqualLength
  should return diff if iterators are different by length and content is different  $diffTextWithDifferentLength
"""

  def equalText = {
    val text1 = List(1,2)
    val text2 = List(1,2)
    val result = TextComparer.compareText(text1, text2)
    result must be equalTo List(
      EqualLine(0, TextLine(0, 1), TextLine(0, 1)),
      EqualLine(1, TextLine(1,2), TextLine(1,2)))
  }

  def diffTextWithEqualLength = {
    val text1 = List(1,2)
    val text2 = List(2,1)
    val result = TextComparer.compareText(text1, text2)
    result must be equalTo List(
      DiffLine(0, TextLine(0, 1), TextLine(0, 2)),
      DiffLine(1, TextLine(1, 2), TextLine(1, 1))
    )
  }

  def diffTextWithDifferentLength = {
    val text1 = List(1,2,3)
    val text2 = List(1,2)
    val result = TextComparer.compareText(text1, text2)
    result must be equalTo List(
      EqualLine(0, TextLine(0, 1), TextLine(0, 1)),
      EqualLine(1, TextLine(1, 2), TextLine(1, 2)),
      DiffLine(2, TextLine(2, 3), EmptyLine(2))
    )
  }
}
