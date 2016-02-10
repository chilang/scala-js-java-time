package org.scalajs.testsuite.javalib.time

import java.time.temporal.{ChronoField, ChronoUnit, UnsupportedTemporalTypeException}
import java.time.{DateTimeException, Instant}

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows

class InstantTest extends TemporalTest[Instant]{

  import AssertThrows._
  import DateTimeTestUtil._
  import Instant._
  import ChronoField._
  import ChronoUnit._

  override def isSupported(unit: ChronoUnit): Boolean = unit == NANOS || unit == MICROS || unit == MILLIS ||
    unit == SECONDS || unit == MINUTES || unit ==HOURS || unit == HALF_DAYS || unit == DAYS
  override def isSupported(field: ChronoField): Boolean = field == NANO_OF_SECOND ||
    field == MICRO_OF_SECOND || field == MILLI_OF_SECOND || field == INSTANT_SECONDS

  override val samples: Seq[Instant] = Seq(MIN, MAX, EPOCH)

  @Test def test_getLong():Unit = {
    assertEquals(0L, MIN.getLong(NANO_OF_SECOND))
    assertEquals(0L, MIN.getLong(MICRO_OF_SECOND))
    assertEquals(0L, MIN.getLong(MILLI_OF_SECOND))
    assertEquals(-31557014167219200L, MIN.getLong(INSTANT_SECONDS))

    assertEquals(999999999L, MAX.getLong(NANO_OF_SECOND))
    assertEquals(999999L, MAX.getLong(MICRO_OF_SECOND))
    assertEquals(999L, MAX.getLong(MILLI_OF_SECOND))
    assertEquals(31556889864403199L, MAX.getLong(INSTANT_SECONDS))

    assertEquals(0L, EPOCH.getLong(NANO_OF_SECOND))
    assertEquals(0L, EPOCH.getLong(MICRO_OF_SECOND))
    assertEquals(0L, EPOCH.getLong(MILLI_OF_SECOND))
    assertEquals(0L, EPOCH.getLong(INSTANT_SECONDS))

  }
  @Test def test_now():Unit = {
    assertNotNull(now())
  }

  @Test def test_ofEpochSecond():Unit = {
    testDateTime(ofEpochSecond(0))(EPOCH)
    testDateTime(ofEpochSecond(0, 0))(EPOCH)
    testDateTime(ofEpochSecond(31556889864403199L, 999999999))(MAX)
    testDateTime(ofEpochSecond(-31557014167219200L, 0))(MIN)
    testDateTime(ofEpochSecond(123))(ofEpochSecond(123,0))
    testDateTime(ofEpochSecond(3, 1))(ofEpochSecond(4, -999999999))
    testDateTime(ofEpochSecond(3, 1))(ofEpochSecond(2, 1000000001))

    expectThrows(classOf[DateTimeException], ofEpochSecond(31556889864403199L + 1))
    expectThrows(classOf[DateTimeException], ofEpochSecond(31556889864403199L, 999999999 + 1))
    expectThrows(classOf[DateTimeException], ofEpochSecond(-31557014167219200L - 1))
    expectThrows(classOf[DateTimeException], ofEpochSecond(-31557014167219200L - 1, -1))
  }

  @Test def test_getNano():Unit = {
    assertEquals(0, EPOCH.getNano)
    assertEquals(0, MIN.getNano)
    assertEquals(999999999, MAX.getNano)
    assertEquals(1, ofEpochSecond(4, -999999999).getNano)
    assertEquals(1, ofEpochSecond(2, 1000000001).getNano)
    assertEquals(234567890, ofEpochSecond(1, 234567890).getNano)
  }

  @Test def test_with():Unit = {
    val supportedFields = Set(NANO_OF_SECOND, MICRO_OF_SECOND, MILLI_OF_SECOND, INSTANT_SECONDS)
    for (t <- samples) {
      for (n <- Seq(0, 999, 999999, 999999999))
        testDateTime(t.`with`(NANO_OF_SECOND, n))(ofEpochSecond(t.getEpochSecond(), n))
      for (n <- Seq(0, 999, 999999))
        testDateTime(t.`with`(MICRO_OF_SECOND, n))(ofEpochSecond(t.getEpochSecond(), n * 1000))
      for (n <- Seq(0, 500, 999))
        testDateTime(t.`with`(MILLI_OF_SECOND, n))(ofEpochSecond(t.getEpochSecond(), n * 1000000))
      for (n <- Seq(-1000000000L, -86400L, -3600L, -60L, -1L, 0L, 1L, 60L, 3600L, 86400L, 1000000000L))
        testDateTime(t.`with`(INSTANT_SECONDS, n))(ofEpochSecond(n, t.getNano()))

      for (f <- ChronoField.values()) {
        if (!supportedFields(f)) expectThrows(classOf[UnsupportedTemporalTypeException], t.`with`(f, 1))
      }

      expectThrows(classOf[DateTimeException], t.`with`(NANO_OF_SECOND, -1))
      expectThrows(classOf[DateTimeException], t.`with`(NANO_OF_SECOND, 999999999+1))
      expectThrows(classOf[DateTimeException], t.`with`(MICRO_OF_SECOND, -1))
      expectThrows(classOf[DateTimeException], t.`with`(MICRO_OF_SECOND, 999999+1))
      expectThrows(classOf[DateTimeException], t.`with`(MILLI_OF_SECOND, -1))
      expectThrows(classOf[DateTimeException], t.`with`(MILLI_OF_SECOND, 999+1))
      expectThrows(classOf[DateTimeException], t.`with`(INSTANT_SECONDS, MIN.getEpochSecond-1))
      expectThrows(classOf[DateTimeException], t.`with`(INSTANT_SECONDS, MAX.getEpochSecond+1))
    }
  }

  @Test def test_truncatedTo():Unit = {
    testDateTime(MIN.truncatedTo(NANOS))(MIN)
    testDateTime(MIN.truncatedTo(MICROS))(MIN)
    testDateTime(MIN.truncatedTo(MILLIS))(MIN)
    testDateTime(MIN.truncatedTo(SECONDS))(MIN)
    testDateTime(MIN.truncatedTo(MINUTES))(MIN)
    testDateTime(MIN.truncatedTo(HOURS))(MIN)
    testDateTime(MIN.truncatedTo(HALF_DAYS))(MIN)
    testDateTime(MIN.truncatedTo(DAYS))(MIN)

    testDateTime(MAX.truncatedTo(NANOS))(MAX)
    testDateTime(MAX.truncatedTo(MICROS))(MAX.minusNanos(999))
    testDateTime(MAX.truncatedTo(MILLIS))(MAX.minusNanos(999999))
    testDateTime(MAX.truncatedTo(SECONDS))(MAX.minusNanos(999999999))
    testDateTime(MAX.truncatedTo(MINUTES))(MAX.minusNanos(999999999).minusSeconds(59))
    testDateTime(MAX.truncatedTo(HOURS))(MAX.minusNanos(999999999).minusSeconds(59).minus(59, MINUTES))
    testDateTime(MAX.truncatedTo(HALF_DAYS))(MAX.minusNanos(999999999).minusSeconds(59).minus(59, MINUTES).minus(11, HOURS))
    testDateTime(MAX.truncatedTo(DAYS))(MAX.minusNanos(999999999).minusSeconds(59).minus(59, MINUTES).minus(23, HOURS))
  }


  @Test def test_ofEpochMilli():Unit = {
    testDateTime(ofEpochMilli(0))(EPOCH)
    testDateTime(ofEpochMilli(1234))(ofEpochSecond(1,234000000))
  }

  @Test def test_from():Unit = {
    for (t <- samples)
      testDateTime(from(t))(t)
  }

  @Test def test_compareTto():Unit = {
    assertEquals(0, MIN.compareTo(MIN))
    assertEquals(0, MAX.compareTo(MAX))
    assertTrue(MAX.compareTo(MIN) > 0)
    assertTrue(MIN.compareTo(MAX) < 0)
  }

  @Test def test_isAfter(): Unit = {
    assertFalse(MIN.isAfter(MIN))
    assertFalse(MIN.isAfter(MAX))
    assertTrue(MAX.isAfter(MIN))
    assertFalse(MAX.isAfter(MAX))
  }

  @Test def test_isBefore(): Unit = {
    assertFalse(MIN.isBefore(MIN))
    assertTrue(MIN.isBefore(MAX))
    assertFalse(MAX.isBefore(MIN))
    assertFalse(MAX.isBefore(MAX))
  }

  @Test def test_adjustInto():Unit = {
    for {
      t1 <- samples
      t2 <- samples
    } {
      testDateTime(t1.adjustInto(t2))(t1)
    }
  }


  @Test def test_until(): Unit = {
    assertEquals(63113904031622399L, MIN.until(MAX, SECONDS) )
    assertEquals(1051898400527039L, MIN.until(MAX, MINUTES))
    assertEquals(17531640008783L, MIN.until(MAX, HOURS))
    assertEquals(1460970000731L, MIN.until(MAX, HALF_DAYS))
    assertEquals(730485000365L, MIN.until(MAX, DAYS))

    for (u <- timeBasedUnits) {
      assertEquals(-MIN.until(MAX, u), MAX.until(MIN, u))
      assertEquals(0L, MIN.until(MIN, u))
      assertEquals(0L, MAX.until(MAX, u))
    }
    for (u <- dateBasedUnits) {
      if (u != DAYS)
        expectThrows(classOf[UnsupportedTemporalTypeException], MIN.until(MIN, u))
    }
  }

  @Test def test_plus():Unit = {
    val values = Seq(Long.MinValue, -1000000000L, -86400L, -3600L, -60L, -1L, 0L,
      1L, 60L, 3600L, 86400L, 1000000000L, Long.MaxValue)

    for {
      t <- samples
      n <- values
    } {
      testDateTime(t.plus(n, NANOS))(t.plusNanos(n))
      testDateTime(t.plus(n, MICROS))(t.plusNanos(n * 1000))
      testDateTime(t.plus(n, MILLIS))(t.plusNanos(n * 1000000))
      testDateTime(t.plus(n, SECONDS))(t.plusSeconds(n))
      testDateTime(t.plus(n, MINUTES))(t.plusSeconds(n * 60))
      testDateTime(t.plus(n, HOURS))(t.plusSeconds(n * 60 * 60))
      testDateTime(t.plus(n, HALF_DAYS))(t.plusSeconds(n * 60 * 60 * 12))
      testDateTime(t.plus(n, DAYS))(t.plusSeconds(n * 60 * 60 * 24))
    }
  }
}
