package com.basselop.bencode

import scala.concurrent.duration.Duration

/**
 * Created by arneball on 2015-11-14.
 */
trait ToBen[T] extends (T ⇒ BEnc)

object ToBen {
  implicit object Identity extends ToBen[BEnc] {
    def apply(v1: BEnc) = v1
  }
  implicit def seq[T: ToBen]: ToBen[Seq[T]] = new ToBen[Seq[T]] {
    override def apply(v1: Seq[T]): BEnc = {
      val values = v1.map { implicitly[ToBen[T]] }
      BList(values: _*)
    }
  }

  implicit def benNum[T](implicit i: Integral[T]) = new ToBen[T] {
    def apply(v1: T) = BInt(i.toLong(v1))
  }
  implicit object BenString extends ToBen[String] {
    def apply(str: String) = BString(str)
  }
  implicit object BenDuration extends ToBen[Duration] {
    def apply(t: Duration) = BInt(t.toSeconds)
  }
}