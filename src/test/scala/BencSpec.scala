import java.io.{ FileNotFoundException, File }

import com.basselop.bencode._
import Implicits._
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

import scala.collection.mutable

class BencSpec extends Specification with Matchers {
  private def parseFile(): BEnc = {
    val fileName = "torrent.torrent"
    val loader = getClass.getClassLoader
    val file = loader.getResource(fileName).getFile
    val decoded = Decoder(new File(file))
    decoded
  }

  private def parseAndRemoveTrackers: BDict = {
    val f = parseFile().asInstanceOf[BDict]
    assert(f.values.get("announce-list").isDefined)
    val newF = f.withoutAnnounce
    newF
  }

  implicit def str2benstr(str: String): BString = new BString(str)

  def apply(str: String) = Decoder.apply(str).map { _._1 }.get

  "A simple torrent parser" should {
    "be able to parse a list" in {
      apply("l4:spam4:eggse") must_=== BList("spam", "eggs")
    }
    "be able to parse an empty list" in {
      apply("le") must_=== BList()
    }
    "be able to parse a dict" in {
      apply("d3:cow3:moo4:spam4:eggse") must_=== BDict(BString("cow") -> BString("moo"), BString("spam") -> BString("eggs"))
    }

    "be able to parse an empty dict" in {
      apply("de") must_=== BDict()
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
      newF.values.get("announce-list").isEmpty must beTrue
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
      Decoder(tempFile) must_=== newF
    }

    "be able to parse BList from Scala Seqs" in {
      val items = List(BInt(1), BString(""))
      val wantedRes: BList = BList(BInt(1), BString(""))
      assert(items.to[({ type T[_] = BList })#T] == wantedRes)
      val newThat: BList = items.map(identity)(collection.breakOut)
      newThat must_=== wantedRes
    }

    "be able to parse BDict from Scala Maps" in {
      val map = Map(BString("Arne") -> BInt(1))
      val dict: BDict = map.map(identity)(collection.breakOut)
      dict.values.get("Arne").collectFirst {
        case i: BInt ⇒ i.value
      }.contains(1L) must beTrue
    }
  }

  "to complete coverage a torrent parser library" should {
    "fail apply method on BString" in {
      new BString("kalle").apply("bla") must throwA[NoSuchElementException]
    }

    "create a Benc using factory method" in {
      BEnc.apply("Kalle" -> BInt(1)) must_=== BDict(new BString("Kalle") -> BInt(1))
    }

    "add announce to bdict" in {
      BDict().withAnnounce("kalle") must_=== BDict(("announce": BString) -> ("kalle": BString))
    }

    "add multiple announce to bdict" in {
      BDict().withAnnounces("a", "b") must_=== BDict(("announce-list": BString) -> BList(BList("a"), BList("b")))
    }

    "add multiple announce to existing announcelist which in turn is bogus" in {
      BDict(("announce-list": BString) -> BInt(2)).withAnnounces("a", "b") must_=== BDict(("announce-list": BString) -> BList(BList("a"), BList("b")))
    }

    "set private to empty Bdict" in {
      BDict().setPrivate must_=== BDict(BString("info") -> BDict(BString("private") -> BInt(1)))
    }

    "set private to Bdict with info field" in {
      BDict(BString("info") -> BDict()).setPrivate must_=== BDict(BString("info") -> BDict(BString("private") -> BInt(1)))
    }

    "BList unapply Stream.Empty == None" in {
      BList.unapply(Stream.empty) must_=== None
    }

    "canBuildFrom apply methods" in {
      BList.cbf.apply(Nil).result() must_== BList()
      BDict.cbf.apply(BDict()).result() must_== BDict()
    }
  }
}