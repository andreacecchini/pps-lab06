package it.unibo.pps.ex1

import org.junit.Test
import org.junit.Assert.*

/** Test for [[List]]. */
class ListTest:
  import List.*
  val l: List[Int] = 1::2::3::4::Nil()
  @Test def testZipWithValue(): Unit =
    val value = 10
    assertEquals((1, 10)::(2,10)::(3,10)::(4,10)::Nil(), l.zipWithValue(10))
  @Test def testLength(): Unit =
    assertEquals(4, l.length())
  @Test def testIndices(): Unit =
    assertEquals(0::1::2::3::Nil(), l.indices())
  @Test def testZipWithIndex(): Unit =
    assertEquals((1, 0)::(2,1)::(3,2)::(4,3)::Nil(), l.zipWithIndex)
  @Test def testPartition(): Unit =
    val (lTrue, lFalse) = l.partition(_ % 2 == 0)
    assertEquals(2 :: 4 :: Nil(), lTrue)
    assertEquals(1::3::Nil(), lFalse)
  @Test def testSpan(): Unit =
    val (untilTrue, rest) = l.span(_ % 2 != 0)
    assertEquals(1::Nil(), untilTrue)
    assertEquals(2::3::4::Nil(), rest)
  @Test def testTakeRight(): Unit =
    assertEquals(2::3::4::Nil(), l.takeRight(3))
end ListTest
