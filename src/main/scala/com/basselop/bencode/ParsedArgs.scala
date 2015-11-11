package com.basselop.bencode

import java.io.{ File, FileNotFoundException }
import java.nio.file.{ Paths, Files }

/**
 * Created by arneball on 2015-11-11.
 */
case class ParsedArgs(f: File = null,
  out: File = null,
  remTrackers: List[String] = Nil,
  remAllTrackers: Boolean = false,
  addTrackers: List[String] = Nil)

object ParsedArgs {
  def parseArgs(args: List[String], p: ParsedArgs = ParsedArgs()): ParsedArgs = args match {
    case Nil ⇒ p
    case ("-f" | "--file") :: file :: rest ⇒
      val path = Paths.get(file)
      if (!Files.exists(path)) {
        throw new FileNotFoundException(file)
      }
      parseArgs(rest, p.copy(f = path.toFile))
    case ("-a" | "--add") :: path :: rest ⇒
      parseArgs(rest, p.copy(addTrackers = path.split(",").toList))
    case ("-o" | "--out") :: out :: rest ⇒
      parseArgs(rest, p.copy(out = new File(out)))
    case ("-r" | "--rem") :: "*" :: rest ⇒
      parseArgs(rest, p.copy(remAllTrackers = true))
    case ("-r" | "--rem") :: that :: rest ⇒
      parseArgs(rest, p.copy(remTrackers = that.split(",").toList))
    case flag :: _ ⇒
      throw new IllegalArgumentException(
        s"""Unknown flag: $flag
           |Usage:
           |  -f: Path to torrent
           |  -o: Output path torrent
           |  -a: add trackers, comma separated
           |  -r: remove trackers, comma separated or *
         """.stripMargin)
  }
}