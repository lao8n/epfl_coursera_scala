package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees { 
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  
  @Test def `times List('a', 'b', 'a'))`: Unit = 
    assertEquals(List(('a', 2), ('b', 1)), times(List('a', 'b', 'a')))

  @Test def `max frequency of list`: Unit = 
    assertEquals(Leaf('a', 2), maxFrequency(List(('a', 2), ('b', 1))))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `decode secret`: Unit = {
    assertEquals(decodedSecret, List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  @Test def `encode secret`: Unit = {
    assertEquals(encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')),
      secret)
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `merge code tables `: Unit = {
    val left = List(('a', List[Bit]()))
    val right = List(('b', List[Bit]()))
    assertEquals(mergeCodeTables(left, right), List(('a', List[Bit](0)), ('b', List[Bit](1))))
  }

  @Test def `convert code tree to code table`: Unit = {
    new TestTrees{
      assertEquals(convert(t1), List(('a', List(0)), ('b', List(1))))
    }
  }

  @Test def `quick Encode test`: Unit = {
    new TestTrees{
      assertEquals(quickEncode(t1)("ab".toList), encode(t1)("ab".toList))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
