package com.basselop.bencode

import scala.collection.immutable
import scala.language.implicitConversions

/**
 * Created by arneball on 2015-11-11.
 */
object Implicits {
  implicit class StrW(val str: String) extends AnyVal {
    def asciiBytes: immutable.Seq[Byte] = str.getBytes("ascii").to[immutable.Seq]
  }
  implicit def str2benstr(str: String): BString = new BString(str)
}