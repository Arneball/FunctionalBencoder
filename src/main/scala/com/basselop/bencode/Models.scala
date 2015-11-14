package com.basselop.bencode

import Implicits._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

sealed abstract class BEnc {
  def toBytes: Seq[Byte]
  def apply(k: BString): BEnc = throw new NoSuchElementException
  final def get(k: BString) = Try { apply(k) }.toOption
  protected final def genBytes[T](start: String, values: Iterable[T])(fun: (mutable.Builder[Byte, Vector[Byte]], T) ⇒ Unit) = {
    val builder = Vector.newBuilder ++= start.asciiBytes
    values.foreach { t ⇒
      fun(builder, t)
    }
    builder ++= BEnd.instance.toBytes
    builder.result()
  }
}

sealed trait BEncCompanion[T <: BEnc] {
  def unapply(chars: Stream[Byte]): Option[(T, Stream[Byte])]
}

object BEnc extends BEncCompanion[BEnc] {
  private val extractors = Stream(BInt, BString, BDict, BList, BEnd)
  def unapply(chars: Stream[Byte]) = extractors.flatMap { _.unapply(chars) }.headOption
}

case class BEnd private () extends BEnc {
  val toBytes = "e".asciiBytes
}
object BEnd extends BEncCompanion[BEnd] {
  private[bencode] val instance = BEnd()
  def unapply(chars: Stream[Byte]) = chars match {
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
  def unapply(chars: Stream[Byte]) = chars match {
    case Digit(digit, _ +: sublist) ⇒
      val (str, rest) = sublist.splitAt(digit)
      Some(new BString(str.toIndexedSeq), rest)
    case _ ⇒ None
  }

  def apply(str: String) = new BString(str)
}

case class BDict(values: Map[BString, BEnc]) extends BEnc {
  def withoutAnnounce = copy(values - "announce" - "announce-list")
  def withoutAnnounce(trackers: List[String]) = {
    val newValues = trackers.foldLeft(values) { _ - _ }
    copy(newValues)
  }

  override def apply(k: BString) = values(k)

  def withAnnounce(str: String) = copy(values + (("announce", str)))
  def withAnnounces(str: String*) = {
    val existingList = values.getOrElse("announce-list", BList())
    val that = str.map { s ⇒ BList(s) } // list of lists of strings
    val newElems = existingList match {
      case bl: BList ⇒ BList(bl.values ++ that: _*)
      case _         ⇒ BList(that: _*)
    }
    copy(values + (("announce-list", newElems)))
  }

  def setPrivate = {
    values.get("info") match {
      case Some(d: BDict) ⇒
        val newInfo = d.copy(d.values + (("private", BInt(1))))
        copy(values + (("info", newInfo)))
      case _ ⇒
        this
    }
  }

  override def toBytes = genBytes("d", values) {
    case (builder, (k, v)) ⇒
      builder ++= k.toBytes ++= v.toBytes
  }
}
object BDict extends BEncCompanion[BDict] {
  private type Acc = Map[BString, BEnc]
  @tailrec private def parsePairs(chars: Stream[Byte], acc: Acc): (BDict, Stream[Byte]) = chars match {
    case BEnd(_, rest)                   ⇒ BDict(acc) -> rest
    case BString(key, BEnc(value, rest)) ⇒ parsePairs(rest, acc + (key -> value))
  }

  def unapply(chars: Stream[Byte]) = chars match {
    case 'd' #:: rest ⇒ Some(parsePairs(rest, Map()))
    case _            ⇒ None
  }

  def apply(args: (BString, BEnc)*) = new BDict(args.toMap)
}

case class BList(values: BEnc*) extends BEnc {
  override def toBytes = genBytes("l", values) { _ ++= _.toBytes }
}
object BList extends BEncCompanion[BList] {
  @tailrec private def doParse(chars: Stream[Byte], acc: List[BEnc] = Nil): (BList, Stream[Byte]) = chars match {
    case BEnd(_, rest)    ⇒ BList(acc.reverse: _*) -> rest
    case BEnc(that, rest) ⇒ doParse(rest, that :: acc)
  }

  def unapply(chars: Stream[Byte]) = chars match {
    case 'l' #:: rest ⇒ Some(doParse(rest))
    case _            ⇒ None
  }
}

case class BInt(value: Long) extends BEnc {
  def toBytes = s"i${value}e".asciiBytes
}
object BInt extends BEncCompanion[BInt] {
  def unapply(chars: Stream[Byte]) = chars match {
    case 'i' #:: rest1 ⇒
      val (int, _ +: rest) = rest1.span('e' != _)
      val string = int.map { _.toChar }.mkString
      val long = string.toLong
      Some(BInt(long), rest)
    case _ ⇒ None
  }
}