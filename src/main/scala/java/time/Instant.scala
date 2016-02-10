package java.time

import java.time.Preconditions._
import java.time.temporal._
import scala.scalajs.js

final class Instant(epochSecond:Long, nanoInSecond:Int) extends Temporal with TemporalAdjuster with Comparable[Instant]{

  import ChronoUnit._
  import ChronoField._
  import Constants._
  import Instant._

  requireDateTime(epochSecond >= MIN_EPOCH_SECOND && epochSecond <= MAX_EPOCH_SECOND, s"epochSecond out of range [$MIN_EPOCH_SECOND, $MAX_EPOCH_SECOND]")
  requireDateTime(nanoInSecond >= 0 && nanoInSecond <= 999999999, s"nanoInSecond out of range [0, 999999999]")

  private def nanos(i:Instant):Long = i.getEpochSecond * NANOS_IN_SECOND + i.getNano

  private val totalNanos: Long = nanos(this)


  def isBefore(o:Instant):Boolean = compareTo(o) < 0
  def isAfter(o:Instant):Boolean = compareTo(o) > 0

  override def adjustInto(temporal: Temporal): Temporal = temporal.`with`(INSTANT_SECONDS, epochSecond).`with`(NANO_OF_SECOND, nanoInSecond)

  override def compareTo(o: Instant): Int = totalNanos.compareTo(nanos(o))

  override def isSupported(unit: TemporalUnit): Boolean = unit match {
    case _:ChronoUnit => unit == NANOS || unit == MICROS || unit == MILLIS || unit == SECONDS || unit == MINUTES || unit ==HOURS || unit == HALF_DAYS || unit == DAYS
    case null => false
    case _ => unit.isSupportedBy(this)
  }

  def plus(amount: Long, unit: TemporalUnit): Instant = unit match {
    case _:ChronoUnit => unit match {
      case NANOS => plusNanos(amount)
      case MICROS => plusNanos(amount * NANOS_IN_MICRO)
      case MILLIS => plusNanos(amount * NANOS_IN_MILLI)
      case SECONDS => plusSeconds(amount)
      case MINUTES => plusSeconds(amount * SECONDS_IN_MINUTE)
      case HOURS => plusSeconds(amount * SECONDS_IN_HOUR)
      case HALF_DAYS => plusSeconds(amount * SECONDS_IN_HOUR * 12)
      case DAYS => plusSeconds(amount * SECONDS_IN_DAY)
      case _ => throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
    }
    case _ => unit.addTo(this, amount)
  }

  def plusNanos(nanosToAdd:Long):Instant = {
    Instant.ofEpochSecond(epochSecond + nanosToAdd/NANOS_IN_SECOND, (nanoInSecond + nanosToAdd) % NANOS_IN_SECOND)
  }

  def plusMillis(millisToAdd:Long):Instant = plusNanos(millisToAdd * NANOS_IN_MILLI)

  def plusSeconds(secondsToAdd:Long):Instant = Instant.ofEpochSecond(epochSecond + secondsToAdd, nanoInSecond)

  override def until(end: Temporal, unit: TemporalUnit): Long = unit match {
    case _:ChronoUnit =>
      val endInstant = from(end)
      unit match {
        case NANOS => diffNanos(endInstant)
        case MICROS => diffNanos(endInstant) / NANOS_IN_MICRO
        case MILLIS => diffNanos(endInstant) / NANOS_IN_MILLI
        case SECONDS => diffSeconds(endInstant)
        case MINUTES => diffSeconds(endInstant) / SECONDS_IN_MINUTE
        case HOURS => diffSeconds(endInstant) / SECONDS_IN_HOUR
        case HALF_DAYS => diffSeconds(endInstant) / (SECONDS_IN_HOUR * 12)
        case DAYS => diffSeconds(endInstant) / SECONDS_IN_DAY
        case _ => throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
      }
    case _ => unit.between(this, end)
  }

  private def diffNanos(end: Instant): Long = end.totalNanos - totalNanos
  private def diffSeconds(end: Instant): Long = end.getEpochSecond - epochSecond


  def `with`(field: TemporalField, newValue: Long): Instant = field match {
    case _:ChronoField => field match {
      case NANO_OF_SECOND => new Instant(epochSecond, newValue.toInt)
      case MICRO_OF_SECOND => new Instant(epochSecond, (newValue * NANOS_IN_MICRO).toInt)
      case MILLI_OF_SECOND => new Instant(epochSecond, (newValue * NANOS_IN_MILLI).toInt)
      case INSTANT_SECONDS => new Instant(newValue, nanoInSecond)
      case _ => throw new UnsupportedTemporalTypeException(s"Field not supported: $field")
    }
    case _ => field.adjustInto(this, newValue)
  }
  override def isSupported(field: TemporalField): Boolean = field match {
    case _:ChronoField  => field == NANO_OF_SECOND || field == MICRO_OF_SECOND || field == MILLI_OF_SECOND || field == INSTANT_SECONDS
    case null           => false
    case _              => field.isSupportedBy(this)
  }
  override def getLong(field: TemporalField): Long = field match {
    case NANO_OF_SECOND => nanoInSecond
    case MICRO_OF_SECOND =>  nanoInSecond / NANOS_IN_MICRO
    case MILLI_OF_SECOND =>  nanoInSecond / NANOS_IN_MILLI
    case INSTANT_SECONDS =>  epochSecond
    case _ => throw new UnsupportedTemporalTypeException(s"Field not supported: $field")
  }

  override def range(field: TemporalField): ValueRange = field match {
    case NANO_OF_SECOND => ValueRange.of(0, NANOS_IN_SECOND-1)
    case MICRO_OF_SECOND => ValueRange.of(0, MICROS_IN_SECOND-1)
    case MILLI_OF_SECOND => ValueRange.of(0, MILLIS_IN_SECOND-1)
    case INSTANT_SECONDS => ValueRange.of(Long.MinValue,Long.MaxValue)
    case _ =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")
  }

  def minusNanos(nanosToSubstract:Long):Instant = plusNanos(-nanosToSubstract)

  def minusMillis(millisToSubstract:Long):Instant = plusMillis(-millisToSubstract)

  def minusSeconds(secondsToSubstract:Long):Instant = plusSeconds(-secondsToSubstract)

  override def get(field: TemporalField): Int = field match {
    case INSTANT_SECONDS => throw new DateTimeException(s"$INSTANT_SECONDS is too large to fit in an Int")
    case _ => getLong(field).toInt
  }

  def minus(amountToSubtract: Long, unit: TemporalUnit): Instant =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  def getNano():Int = nanoInSecond

  def getEpochSecond():Long = epochSecond

  def toEpochMilli():Long = epochSecond * MICROS_IN_SECOND + (nanoInSecond / NANOS_IN_MILLI)

  override def toString: String = s"Instant(seconds:$epochSecond, nanoInSecond:$nanoInSecond"

  override def equals(obj: scala.Any): Boolean = obj match {
    case o:Instant => epochSecond == o.getEpochSecond && nanoInSecond == o.getNano
    case _ => false
  }
  override def hashCode(): Int = totalNanos.hashCode()

  def truncatedTo(unit:TemporalUnit):Instant = {
    if (unit == NANOS) this
    else if (unit.isTimeBased || unit == DAYS) {
      val duration = unit.getDuration
      val seconds = duration.getSeconds
      if (seconds > 0) Instant.ofEpochSecond(epochSecond - (epochSecond % seconds))
      else Instant.ofEpochSecond(epochSecond, nanoInSecond - (nanoInSecond % duration.getNano))
    } else throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
  }
}
object Instant {
  import ChronoUnit._
  import ChronoField._
  import Constants._

  private val MIN_EPOCH_SECOND: Long = -31557014167219200L
  private val MAX_EPOCH_SECOND: Long = 31556889864403199L

  val MIN:Instant = new Instant(MIN_EPOCH_SECOND,0)
  val MAX:Instant = new Instant(MAX_EPOCH_SECOND,999999999)

  val EPOCH:Instant = new Instant(0,0)


  def now():Instant = {
    val d = new js.Date()
    val millis: Double = d.getTime
    new Instant((millis / 1000).toLong, (millis % 1000 * 1000).toInt)
  }

  def ofEpochSecond(epochSecond:Long):Instant = new Instant(epochSecond,0)

  def ofEpochSecond(epochSecond:Long, nanoAdjustment:Long):Instant = {
    val mod = nanoAdjustment % NANOS_IN_SECOND
    if (mod <0) new Instant(epochSecond - (nanoAdjustment/NANOS_IN_SECOND) - 1, (NANOS_IN_SECOND + mod).toInt)
    else new Instant(epochSecond + (nanoAdjustment/NANOS_IN_SECOND), mod.toInt)
  }

  private def ofNano(nanos:Long):Instant = {
    ofEpochSecond(nanos / NANOS_IN_SECOND, nanos % NANOS_IN_SECOND)
  }

  def ofEpochMilli(epochMilli:Long):Instant =
    new Instant(epochMilli / MILLIS_IN_SECOND, ((epochMilli % MILLIS_IN_SECOND) * NANOS_IN_MILLI).toInt)

  def from(temporal:TemporalAccessor): Instant = {
    Instant.ofEpochSecond(temporal.getLong(INSTANT_SECONDS), temporal.getLong(NANO_OF_SECOND))
  }

  //Not implemented
  //def parse(text:CharSequence):Instant

}