package com.github.comparer

/**
  * Created by stremlenye on 04/04/16.
  */
object Implicits {
  implicit def compareNumeric[A](n1: A, n2: A)(implicit numeric: Numeric[A]): Boolean = numeric.equiv(n1,n2)
  implicit def compareStrings(s1: String, s2: String): Boolean = s1 == s2
}
