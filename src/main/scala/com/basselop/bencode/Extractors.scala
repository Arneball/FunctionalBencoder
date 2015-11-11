package com.basselop.bencode

/**
 * Created by arneball on 2015-11-11.
 */
object Digit {
  def unapply(c: Stream[Byte]): Option[(Int, Stream[Byte])] = {
    val (a, b) = c.span(_.toChar.isDigit)
    def aStr = a.map { _.toChar }.mkString.toInt
    if (a.isEmpty) None else Some(aStr -> b)
  }
}