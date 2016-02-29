import com.github.comparer.Comparer
import com.github.comparer.Comparer.{EmptyLine, TextLine, MatchLine}
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
    val result = Comparer.compareText(text1, text2)
    result must be equalTo List(
      MatchLine(0, TextLine(0, 1), TextLine(0, 1)),
      MatchLine(1, TextLine(1,2), TextLine(1,2)))
  }

  def diffTextWithEqualLength = {
    val text1 = List(1,2)
    val text2 = List(2,1)
    val result = Comparer.compareText(text1, text2)
    result must be equalTo List(
      MatchLine(0, TextLine(0, 1), EmptyLine(0)),
      MatchLine(0, EmptyLine(0), TextLine(0, 2)),
      MatchLine(1, TextLine(1, 2), EmptyLine(1)),
      MatchLine(1, EmptyLine(1), TextLine(1, 1))
    )
  }

  def diffTextWithDifferentLength = {
    val text1 = List(1,2,3)
    val text2 = List(1,2)
    val result = Comparer.compareText(text1, text2)
    result must be equalTo List(
      MatchLine(0, TextLine(0, 1), TextLine(0, 1)),
      MatchLine(1, TextLine(1, 2), TextLine(1, 2)),
      MatchLine(2, TextLine(2, 3), EmptyLine(2))
    )
  }
}