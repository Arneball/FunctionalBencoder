package com.basselop.bencode

import scala.concurrent.duration.Duration

/**
 * Created by arneball on 2015-11-14.
 */
trait ToBen[T] extends (T ⇒ BEnc)

object ToBen {

  def apply(values: (String, BEnc)*) = {
    val map = values.map {
      case (k, v) ⇒
        BString(k) -> v
    }.toMap
    new BDict(map)
  }

  def mk(v: (String, Wrapped[_])*) = {
    val mapped = v.map { case (a, b) ⇒ a -> b.ben }
    apply(mapped: _*)
  }

  implicit class Wrapped[T: ToBen](t: T) {
    val ben = implicitly[ToBen[T]].apply(t)
  }

  implicit object BenInt extends ToBen[Long] {
    def apply(l: Long) = BInt(l)
  }
  implicit object BenString extends ToBen[String] {
    def apply(str: String) = BString(str)
  }
  implicit object BenDuration extends ToBen[Duration] {
    override def apply(t: Duration) = BInt(t.toSeconds)
  }
}