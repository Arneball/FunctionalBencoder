import java.io.{ FileNotFoundException, File }

import com.basselop.bencode._
import Implicits._
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification
import BEnc.{ dict, list }
import scala.collection.mutable

class BencSpec extends Specification with Matchers {
  private def parseFile(): BEnc = {
    val file: String = getTestTorrent
    val decoded = Decoder(new File(file))
    decoded
  }

  private def getTestTorrent: String = {
    val loader = getClass.getClassLoader
    val fileName = "torrent.torrent"
    val file = loader.getResource(fileName).getFile
    file
  }

  private def parseAndRemoveTrackers: BDict = {
    val f = parseFile().asInstanceOf[BDict]
    assert(f.values.get("announce-list").isDefined)
    val newF = f.withoutAnnounce
    newF
  }

  def apply(str: String): BEnc = Decoder.apply(str).map { _._1 }.get

  "A simple torrent parser" should {
    "be able to parse a list" in {
      apply("l4:spam4:eggse") ==== list("spam", "eggs")
    }
    "be able to parse an empty list" in {
      apply("le") ==== BList()
    }
    "be able to parse a dict" in {
      apply("d3:cow3:moo4:spam4:eggse") ==== dict("cow" -> "moo", "spam" -> "eggs")
    }

    "be able to parse an empty dict" in {
      apply("de") ==== BDict()
    }

    "be able to parse a torrent file" in {
      val decoded = parseFile()
      decoded mustNotEqual BDict()
    }

    "throw FileNotFoundException" in {
      Decoder(new File("/tmp/jadda.torrent")) must throwA[FileNotFoundException]
    }
    "be able to remove trackers" in {
      val newF: BDict = parseAndRemoveTrackers
      newF.values.get("announce-list") must beEmpty
    }

    "have a proper `piece length`" in {
      val d = parseFile() match {
        case b: BDict ⇒ b
      }
      val that = d.get("info").flatMap {
        case d: BDict ⇒ d.get("piece length")
        case _        ⇒ None
      }.collect {
        case b: BInt ⇒ b.value
      }
      that.exists(0 < _) must beTrue
    }

    "be able to remove trackers, save and reparse" in {
      val newF = parseAndRemoveTrackers
      val tempFile = File.createTempFile("pooh", "pooh")
      tempFile.deleteOnExit()
      Decoder(tempFile) = newF
      Decoder(tempFile) ==== newF
    }

    "be able to parse BList from Scala Seqs" in {
      val items = List(BInt(1), BString(""))
      val wantedRes: BList = BList(BInt(1), BString(""))
      assert(items.to[({ type T[_] = BList })#T] == wantedRes)
      val newThat: BList = items.map(identity)(collection.breakOut)
      newThat ==== wantedRes
    }

    "be able to parse BDict from Scala Maps" in {
      val map = Map("Arne" -> BInt(1))
      val dict: BDict = map.map(identity)(collection.breakOut)
      dict.values.get("Arne").collectFirst {
        case i: BInt ⇒ i.value
      }.contains(1L) must beTrue
    }
  }

  "to complete coverage a torrent parser library" should {
    "add announce to bdict" in {
      BDict().withAnnounce("kalle") ==== dict("announce" -> "kalle")
    }

    "add multiple announce to bdict" in {
      BDict().withAnnounces("a", "b") ==== dict("announce-list" -> list(list("a"), list("b")))
    }

    "add multiple announce to existing announcelist which in turn is bogus" in {
      dict("announce-list" -> 2).withAnnounces("a", "b") ==== dict("announce-list" -> list(list("a"), list("b")))
    }

    "set private to empty Bdict" in {
      BDict().setPrivate ==== dict("info" -> dict("private" -> 1))
    }

    "set private to Bdict with info field" in {
      dict("info" -> dict()).setPrivate ==== dict("info" -> dict("private" -> 1))
    }

    "BList unapply Stream.Empty == None" in {
      BList.unapply(Stream.empty) ==== None
    }

    "canBuildFrom apply methods" in {
      BDict.cbf.apply(BDict()).result() ==== BDict()
      BList.cbf.apply(Nil).result() ==== BList()
    }

    "BDuration apply method" in {
      import concurrent.duration._
      ToBen.BenDuration(10 seconds) ==== BInt(10)
    }

    "ToBen[Seq]" in {
      import BEnc._
      ToBen.seq[Int].apply(List(1, 2, 3)) ==== list(1, 2, 3)
    }

    "ParsedArgs with most params set " in {
      val pargs = List("-f", getTestTorrent, "-a", "bla", "-o", "bla", "-r", "bla")
      var args = ParsedArgs.parseArgs(pargs)
      args.f mustNotEqual null
      args = ParsedArgs.parseArgs(List("-f", getTestTorrent, "-r", "*", "-o", "/tmp/bla"))
      args.remAllTrackers must beTrue
    }

    "ParsedArgs with faulty file" in {
      ParsedArgs.parseArgs(List("-f", "Slask")) must throwA[NoSuchElementException]
    }

    "ParsedArg with faulty flag" in {
      ParsedArgs.parseArgs(List("-katt", "tre")) must throwA[NoSuchElementException]
    }

    "Add announces to existing list" in {
      dict("announce-list" -> list()).withAnnounces("a", "b") ==== dict("announce-list" -> list(list("a"), list("b")))
    }

    "Use overloaded constructor" in {
      list("a", "b", "c") ==== BList(BString("a"), BString("b"), BString("c"))
    }
  }

  "a torrent parser" should {
    "go both ways" in {
      val that = BEnc.dict("arne" -> 3, "slask" -> BEnc.list(1, 2))
      BEnc.unapply(that.toBytes.toStream).get._1 ==== that
    }
  }
}