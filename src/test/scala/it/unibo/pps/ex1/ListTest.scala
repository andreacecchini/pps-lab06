package it.unibo.pps.ex1

import org.junit.Test
import org.junit.Assert.*

/** Test for [[List]]. */
class ListTest:
  import List.*
  val l: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil()
  @Test def testZipWithValue(): Unit =
    val value = 10
    assertEquals((1, 10)::(2,10)::(3,10)::(4,10)::Nil(), l.zipWithValue(10))
  @Test def testLength(): Unit =
    assertEquals(4, l.length())
end ListTest
