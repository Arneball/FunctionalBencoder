import java.io.{ FileNotFoundException, File }

import com.basselop.bencode._
import Implicits._
import org.scalatest._

class BencSpec extends FlatSpec with Matchers {
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

  def apply(str: String) = Decoder.apply(str).map { _._1 }.get

  "A simple torrent parser" should "be able to parse a list" in {
    assert(apply("l4:spam4:eggse") === BList("spam", "eggs"))
  }

  it should "be able to parse an empty list" in {
    assert(apply("le") === BList())
  }

  it should "be able to parse a dict" in {
    assert(apply("d3:cow3:moo4:spam4:eggse") === BDict(BString("cow") -> BString("moo"), BString("spam") -> BString("eggs")))
  }

  it should "be able to parse an empty dict" in {
    assert(apply("de") === BDict())
  }

  it should "be able to parse a torrent file" in {
    val decoded = parseFile()
    assert(decoded != BDict())
  }

  it should "throw FileNotFoundException" in {
    intercept[FileNotFoundException] {
      Decoder(new File("/tmp/jadda.torrent"))
    }
  }

  it should "be able to remove trackers" in {
    val newF: BDict = parseAndRemoveTrackers
    assert(newF.values.get("announce-list").isEmpty)
  }

  it should "have a proper `piece length`" in {
    val d = parseFile() match {
      case b: BDict ⇒ b
    }
    val that = d.get("info").flatMap {
      case d: BDict ⇒ d.get("piece length")
      case _        ⇒ None
    }.collect {
      case b: BInt ⇒ b.value
    }
    assert {
      that.exists(0 < _)
    }
  }

  it should "be able to remove trackers, save and reparse" in {
    val newF = parseAndRemoveTrackers
    val tempFile = File.createTempFile("pooh", "pooh")
    tempFile.deleteOnExit()
    Decoder(tempFile) = newF
    assert(Decoder(tempFile) === newF)
  }

  it should "be able to parse BList from Scala Seqs" in {
    val items = List(BInt(1), BString(""))
    val wantedRes: BList = BList(BInt(1), BString(""))
    assert(items.to[({ type T[_] = BList })#T] == wantedRes)
    assert {
      val newThat: BList = items.map(identity)(collection.breakOut)
      newThat == wantedRes
    }
  }

  it should "be able to parse BDict from Scala Maps" in {
    val map = Map(BString("Arne") -> BInt(1))
    val dict: BDict = map.map(identity)(collection.breakOut)
    assert {
      dict.values.get("Arne").collectFirst {
        case i: BInt ⇒ i.value
      }.contains(1L)
    }
  }
}