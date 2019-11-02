import org.junit.Test
import org.junit.Assert._
import utils.{given, _}

class RatTests
    @Test def construction(): Unit =
        val r1: Rat = 1
        assertEquals(Rat(1,1), r1)
        assertEquals("1/1", r1.toString)
        val r2 = Rat(3,6)
        assertEquals("1/2", r2.toString)
        val r3 = r2 * 3
        assertEquals(3, r3.numer)
        assertEquals(2, r3.denom)
        val r4 = r3 - 1
        assertEquals(Rat(1,2), r4)
        val r5 = Rat(1,4) + Rat(1,5)
        assertEquals(Rat(9,20), r5)
        val r6 = -r5
        assertEquals(-9, r6.numer)
        assertEquals(Rat(-9, 20), r6)
        val r7 = r5 / 3
        assertEquals(Rat(3,20), r7)
        val b1 = Rat(4/5) < Rat(3/4)
        assertFalse(b1)
        val r8 = r6 max r7
        assertEquals(r7, r8)