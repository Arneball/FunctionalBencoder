package com.basselop.bencode

import com.basselop.bencode.Implicits._

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ IndexedSeq, Seq }
import scala.collection.mutable
import scala.language.implicitConversions

sealed abstract class BEnc {
  /**
   * Convert this instance to a `Array[Byte]`
   *
   * @return a bencoded serialized version
   */
  def toBytes: Seq[Byte]

  /**
   * Convenience function
   *
   * @param start value that delimits the start of the given list or dict
   * @param values the values that should be serialized to bencoded form
   * @return a sequence of bytes
   */
  protected final def genBytes[T](start: String, values: Iterable[T])(fun: (mutable.Builder[Byte, Seq[Byte]], T) ⇒ Unit): Seq[Byte] = {
    val builder = Vector.newBuilder ++= start.asciiBytes
    values.foreach { t ⇒
      fun(builder, t)
    }
    builder ++= BEnd.instance.toBytes
    builder.result()
  }
}

/**
 * Companion trait
 * defines unapply method functionality
 */
sealed abstract class BEncCompanion[+T <: BEnc] {
  protected type Bytes = Stream[Byte]
  /**
   * Tries to extract a T from a Stream[Byte]
   *
   * @param chars
   * @return if successfull a Some[(T, Stream[Byte])] where the second element is the remaining stream of bytes
   */
  def unapply(chars: Bytes): Option[(T, Bytes)]
}

object BEnc extends BEncCompanion[BEnc] {
  private val extractors: Stream[BEncCompanion[BEnc]] = Stream(BInt, BString, BDict, BList, BEnd)

  def list(v: Wrapped*): BList = mkColl(v: _*)(_.ben)(BList.cbf)

  def dict(v: (String, Wrapped)*): BDict = mkColl(v: _*) { case (a, b) ⇒ a -> b.ben }(BDict.cbf)

  def unapply(chars: Bytes): Option[(BEnc, Bytes)] = extractors.flatMap { _.unapply(chars) }.headOption

  final case class Wrapped(ben: BEnc) extends AnyVal

  private def mkColl[E, C, T](stuffs: E*)(f: E ⇒ T)(cbf: CanBuildFrom[Nothing, T, C]) = {
    stuffs.foldLeft(cbf()) { _ += f(_) }.result()
  }

  implicit def wrap(t: BEnc): Wrapped = Wrapped(t)

  implicit def wrap[T: ToBen](t: T): Wrapped =
    Wrapped(implicitly[ToBen[T]].apply(t))
}

case class BEnd private () extends BEnc {
  val toBytes = "e".asciiBytes
}
object BEnd extends BEncCompanion[BEnd] {
  private[bencode] val instance = BEnd()
  def unapply(chars: Bytes) = chars match {
    case 'e' #:: rest ⇒ Some(instance, rest)
    case _            ⇒ None
  }
}

case class BString(value: Seq[Byte]) extends BEnc {
  def this(str: String) = this(str.asciiBytes)
  def realStr: String = value.map { _.toChar }(collection.breakOut)
  override def toString = realStr
  override def toBytes: Seq[Byte] = s"${value.length}:".asciiBytes ++ value
}
object BString extends BEncCompanion[BString] {
  def unapply(chars: Bytes) = chars match {
    case Digit(digit, _ #:: sublist) ⇒
      val (str, rest) = sublist.splitAt(digit)
      Some(new BString(str.toIndexedSeq), rest)
    case _ ⇒ None
  }

  def apply(str: String) = new BString(str)
}

case class BDict(values: Map[String, BEnc]) extends BEnc {
  def withoutAnnounce = copy(values - "announce" - "announce-list")

  def get(key: String) = values.get(key)

  def withAnnounce(str: String) = copy(values + (("announce", str)))
  def withAnnounces(str: String*) = {
    val that = str.map { s ⇒ BList(s) } // list of lists of strings
    val newElems = values.get("announce-list") match {
      case Some(bl: BList) ⇒ BList(bl.values ++ that)
      case _               ⇒ BList(that.toVector)
    }
    copy(values + (("announce-list", newElems)))
  }

  def setPrivate() = values.get("info") match {
    case Some(d: BDict) ⇒
      val newInfo = d.copy(d.values + privateProperty)
      copy(values + (("info", newInfo)))
    case _ ⇒
      copy(values + (("info", makePrivateDict)))
  }

  override def toBytes = genBytes("d", values) {
    case (builder, (k, v)) ⇒
      builder ++= k.toBytes ++= v.toBytes
  }

  private def privateProperty = "private" -> BInt(1)
  private def makePrivateDict = BDict(privateProperty)
}

object BDict extends BEncCompanion[BDict] {
  private type Acc = mutable.Builder[(String, BEnc), Map[String, BEnc]]

  @tailrec private def parsePairs(chars: Bytes, acc: Acc): (BDict, Bytes) = chars match {
    case BEnd(_, rest)                   ⇒ BDict(acc.result()) -> rest
    case BString(key, BEnc(value, rest)) ⇒ parsePairs(rest, acc += (key.realStr -> value))
  }

  def unapply(chars: Bytes) = chars match {
    case 'd' #:: rest ⇒ Some(parsePairs(rest, Map.newBuilder))
    case _            ⇒ None
  }

  def apply(args: (String, BEnc)*) = new BDict(args.toMap)

  implicit object cbf extends Cbf[BDict, (String, BEnc), BDict] {
    override def apply() = Map.newBuilder[String, BEnc].mapResult { new BDict(_) }
  }
}

case class BList private[bencode] (values: IndexedSeq[BEnc]) extends BEnc {
  override def toBytes = genBytes("l", values) { _ ++= _.toBytes }
}
object BList extends BEncCompanion[BList] {
  @tailrec private def doParse(chars: Bytes, acc: mutable.Builder[BEnc, IndexedSeq[BEnc]]): (BList, Bytes) = chars match {
    case BEnd(_, rest)    ⇒ BList(acc.result()) -> rest
    case BEnc(that, rest) ⇒ doParse(rest, acc += that)
  }

  val empty = new BList(Vector.empty)

  def apply() = empty
  def apply(value: BEnc) = new BList(Vector(value))
  def apply(v1: BEnc, v2: BEnc) = new BList(Vector(v1, v2))
  def apply(v1: BEnc, v2: BEnc, values: BEnc*) = new BList(Vector(v1, v2) ++ values)

  def unapply(chars: Bytes) = chars match {
    case 'l' #:: rest ⇒ Some(doParse(rest, Vector.newBuilder))
    case _            ⇒ None
  }

  implicit object cbf extends Cbf[Seq[BEnc], BEnc, BList] {
    def apply() = Vector.newBuilder[BEnc].mapResult(new BList(_))
  }
}

case class BInt(value: Long) extends BEnc {
  def toBytes = s"i${value}e".asciiBytes
}
object BInt extends BEncCompanion[BInt] {
  def unapply(chars: Bytes) = chars match {
    case 'i' #:: rest1 ⇒
      val (int, _ #:: rest) = rest1.span('e' != _)
      val string = int.map { _.toChar }.mkString
      val long = string.toLong
      Some(BInt(long), rest)
    case _ ⇒ None
  }
}

abstract class Cbf[From, Elem, To] extends CanBuildFrom[From, Elem, To] {
  def apply(f: From) = apply()
}