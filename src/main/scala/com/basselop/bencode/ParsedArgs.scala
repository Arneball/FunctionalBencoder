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
  def parseArgs(args: List[String]) = parse.parse(args, ParsedArgs()).get

  private val parse = new scopt.OptionParser[ParsedArgs]("Bencoder") {
    head("Bencoder", "1.0")
    opt[Seq[String]]('a', "add") action { (x, a) ⇒
      a.copy(addTrackers = a.addTrackers ++ x)
    } text "Trackers to add" valueName "<url1>,<url2>"

    opt[File]('o', "out") action { (f, a) ⇒ a.copy(out = f) } text "Outputfile" required ()

    opt[File]('f', "file") action { (f, a) ⇒ a.copy(f = f) } validate { f ⇒
      if (f.exists()) success else failure(s"Input file must exist $f")
    } text "Inputfile" required ()

    opt[String]('r', "rem") action {
      case ("*", a) ⇒ a.copy(remAllTrackers = true)
      case (x, a)   ⇒ a.copy(remTrackers = x.split(",").toList)
    } text "Remove trackers"
  }
}