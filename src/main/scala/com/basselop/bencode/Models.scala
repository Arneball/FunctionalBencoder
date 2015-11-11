package com.basselop.bencode

import Implicits._

import scala.annotation.tailrec

sealed trait BEnc {
  def toBytes: Seq[Byte]
}

sealed trait BEncCompanion[T <: BEnc] {
  type Ret = Option[(T, Stream[Byte])]
  def unapply(chars: Stream[Byte]): Ret
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
    case Digit(digit, _colon #:: sublist) ⇒
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
  def withAnnounce(str: String) = copy(values + (("announce", str)))
  def withAnnounces(str: String*) = {
    val that = str.map { s ⇒ BList(s) } // list of lists of strings
    copy(values + (("announce-list", BList(that: _*))))
  }

  override def toBytes: Seq[Byte] = {
    val builder = values.foldLeft(Vector.newBuilder ++= "d".asciiBytes) {
      case (acc, (k, v)) ⇒ acc ++= k.toBytes ++= v.toBytes
    } ++= BEnd.instance.toBytes
    builder.result
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
  override def toBytes: Seq[Byte] = {
    val builder = values.foldLeft(Vector.newBuilder ++= "l".asciiBytes.toVector) {
      _ ++= _.toBytes
    } ++= BEnd.instance.toBytes
    builder.result()
  }
}
object BList extends BEncCompanion[BList] {
  @tailrec private def doParse(chars: Stream[Byte], acc: List[BEnc] = Nil): (BList, Stream[Byte]) = chars match {
    case BEnd(_, rest)    ⇒ BList(acc.reverse: _*) -> rest
    case BEnc(that, rest) ⇒ doParse(rest, that :: acc)
  }

  def unapply(chars: Stream[Byte]) = chars match {
    case 'l' #:: rest ⇒
      Some(doParse(rest))
    case _ ⇒ None
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