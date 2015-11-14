package com.basselop.bencode

import scala.concurrent.duration.Duration

/**
 * Created by arneball on 2015-11-14.
 */
trait ToBen[T] extends (T ⇒ BEnc)

object ToBen {
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