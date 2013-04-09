package dhg.util

import scala.collection.generic.CanBuildFrom

object Pattern {

  object UInt {
    val IntRE = """^(-?\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }
  //  implicit def int2unapplyInt(objA: Int.type) = UInt

  object UDouble {
    val DoubleRE = """^(-?\d+\.?\d*|-?\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }
  //  implicit def double2unapplyDouble(objA: Double.type) = UDouble

  object UBoolean {
    val booleanRE = """([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])""".r
    def unapply(v: String): Option[Boolean] = v match {
      case booleanRE(s) => Some(s.toBoolean)
      case _ => None
    }
  }

  object UMap {
    def unapplySeq[A, B](m: Map[A, B]): Option[Seq[(A, B)]] = Some(m.toIndexedSeq)
  }

  object USet {
    def unapplySeq[A](s: Set[A]): Option[Seq[A]] = Some(s.toIndexedSeq)
  }

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = {
      Some(pair)
    }
  }

  object Range {
    val RangeRE = """^(\d+)-(\d+)$""".r
    def unapply(s: String): Option[Seq[Int]] = Some(
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeRE(UInt(b), UInt(e)) if b <= e => b to e
      })
  }

  class Range(max: Int) {
    val OpenRangeRE = """^(\d+)-$""".r
    def unapply(s: String): Option[Seq[Int]] = Some(
      s.replaceAll("\\s+", "").split(",").flatMap {
        case OpenRangeRE(UInt(b)) => b to max
        case Range(r) => r
      })
  }

  def makeRangeString(seq: Seq[Int]): String = {
    assert(seq.nonEmpty, "cannot make empty sequence into a range string")
    assert(seq.exists(_ >= 0), s"negative numbers are not permitted: $seq")
    (-2 +: seq).sliding(2).foldLeft(Vector[Vector[Int]]()) {
      case ((z :+ c), Seq(a, b)) =>
        if (a != b - 1)
          (z :+ c) :+ Vector(b)
        else
          (z :+ (c :+ b))
      case (z, Seq(a, b)) =>
        z :+ Vector(b)
    }
      .map {
        case Seq(x) => x.toString
        case s => s.head + "-" + s.last
      }.mkString(",")
  }

  object Iterable {
    def unapplySeq[T](s: Iterable[T]): Option[Seq[T]] =
      Some(s.toIndexedSeq)
  }

}
