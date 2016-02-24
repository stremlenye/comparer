import com.github.comparer.Comparer
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
    val text1 = List("1","2","3").toIterator
    val text2 = List("1","2","3").toIterator
    val result = Comparer.compareLines(text1, text2)
    result.toList must beEmpty
  }

  def diffTextWithEqualLength = {
    val text1 = List("1","2","3").toIterator
    val text2 = List("1","2","2").toIterator
    val result = Comparer.compareLines(text1, text2).toList
    result.length must equalTo(1)
    result.head must equalTo(Some(("3", 3)), Some(("2", 3)))
  }

  def diffTextWithDifferentLength = {
    val text1 = List("1","2","3").toIterator
    val text2 = List("1","2").toIterator
    val result = Comparer.compareLines(text1, text2).toList
    result.length must equalTo(1)
    result.head must equalTo((Some(("3",3)), None))
  }
}
