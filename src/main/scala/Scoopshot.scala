
object Scoopshot extends App {

  def buildStream(stream: Stream[Char]): Stream[String] = new String(stream.take(7).toArray) #:: buildStream(stream.tail)

  def time[T](msg: String)(f: =>T): T = {
    val start = System.currentTimeMillis
    val result = f
    val end = System.currentTimeMillis
    println("%s: %s ms".format(msg, (end - start)))
    result
  }

  import ComputePi.pi

  val stream = time("Computing pi")(buildStream(pi).filter(s => s == s.reverse).filter(n => (2 until n.toInt/2) forall (n.toInt % _ != 0)))
  println(stream.head + "@scoopshot.com")
}

object DownloadPi {
  def pi: Stream[Char] = {
    val source = io.Source.fromURL("http://newton.ex.ac.uk/research/qsystems/collabs/pi/pi6.txt")
    source.getLines().flatMap(_.replaceAll("""\s+""", "")).filter(_.isDigit).drop(1).toStream
  }
}

// adapted from http://shootout.alioth.debian.org/u64q/program.php?test=pidigits&lang=scala&id=3
// which was adapted from http://www.cs.ox.ac.uk/jeremy.gibbons/publications/spigot.pdf
object ComputePi {

  import BigInt._

  def pi: Stream[Char] = {
    digits(new LFT(1, 0, 1), 1)
  }

  private def digits(z: LFT, k: Int): Stream[Char] = z.extract match {
    case Some(y) => Stream.cons((y + '0').toChar, digits(z.next(y), k))
    case None => digits(z.compose(k), k + 1)
  }

  private class LFT(q: BigInt, r: BigInt, t: BigInt) {
    def compose(k: Int): LFT = {
      new LFT(q * k, (q * (4 * k + 2)) + (r * (2 * k + 1)), t * (2 * k + 1))
    }

    def extract: Option[Int] = {
      val (y: BigInt, rem: BigInt) = (q * 3 + r) /% t
      if ((rem + q) < t) {
        Some(y.intValue)
      } else {
        None
      }
    }

    def next(y: Int) = {
      new LFT(q * 10, (r - (t * y)) * 10, t)
    }
  }
}
