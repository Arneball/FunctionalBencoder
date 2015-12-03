package com.basselop.bencode

import java.io._

import scala.language.implicitConversions

/**
 * Created by arneball on 2015-11-10.
 */

object TorrentDecoder {
  def main(args: Array[String]): Unit = {
    val arg @ ParsedArgs(path, out, remtrackers, remAll, addTrackers) = ParsedArgs.parseArgs(args.toList)
    val result = Decoder(path).asInstanceOf[BDict]
    val newRes = (remAll, remtrackers) match {
      case (true, _) ⇒ result.withoutAnnounce
    }
    val newRes2 = newRes.withAnnounces(addTrackers: _*)
    val outPath = out match {
      case null ⇒ path
      case _    ⇒ out
    }
    Decoder(outPath) = newRes2
  }
}

object Decoder {
  def apply(str: String) = BEnc.unapply(str.toStream.map { _.toByte })

  def update(f: File, that: BEnc) = {
    val stream = new FileOutputStream(f)
    try {
      stream.write(that.toBytes.toArray)
    } finally stream.close()
  }

  private def withStream(f: Stream[Byte], c: Closeable) = {
    try {
      BEnc.unapply(f).get._1
    } finally c.close()
  }

  def apply(i: InputStream): BEnc = {
    def mkStream: Stream[Byte] = i.read() match {
      case -1 ⇒ Stream.empty
      case n  ⇒ n.toByte #:: mkStream
    }
    withStream(mkStream, i)
  }

  def apply(f: File): BEnc = apply(new BufferedInputStream(new FileInputStream(f)))
}