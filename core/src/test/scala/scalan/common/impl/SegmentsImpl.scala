package scalan.common

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SegmentsAbs extends Segments with scalan.Scalan {
  self: SegmentsDsl =>

  // single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment = {
    proxyOps[Segment](p)(scala.reflect.classTag[Segment])
  }

  // familyElem
  class SegmentElem[To <: Segment]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[Segment].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Segment] => convertSegment(x) }
      tryConvert(element[Segment], this, x, conv)
    }

    def convertSegment(x: Rep[Segment]): Rep[To] = {
      x.selfType1 match {
        case _: SegmentElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SegmentElem[_], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def segmentElement: Elem[Segment] =
    cachedElem[SegmentElem[Segment]]()

  implicit case object SegmentCompanionElem extends CompanionElem[SegmentCompanionAbs] {
    lazy val tag = weakTypeTag[SegmentCompanionAbs]
    protected def getDefaultRep = Segment
  }

  abstract class SegmentCompanionAbs extends CompanionDef[SegmentCompanionAbs] with SegmentCompanion {
    def selfType = SegmentCompanionElem
    override def toString = "Segment"
  }
  def Segment: Rep[SegmentCompanionAbs]
  implicit def proxySegmentCompanionAbs(p: Rep[SegmentCompanionAbs]): SegmentCompanionAbs =
    proxyOps[SegmentCompanionAbs](p)

  abstract class AbsInterval
      (start: Rep[Int], end: Rep[Int])
    extends Interval(start, end) with Def[Interval] {
    lazy val selfType = element[Interval]
  }
  // elem for concrete class
  class IntervalElem(val iso: Iso[IntervalData, Interval])
    extends SegmentElem[Interval]
    with ConcreteElem[IntervalData, Interval] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertSegment(x: Rep[Segment]) = Interval(x.start, x.end)
    override def getDefaultRep = Interval(0, 0)
    override lazy val tag = {
      weakTypeTag[Interval]
    }
  }

  // state representation type
  type IntervalData = (Int, Int)

  // 3) Iso for concrete class
  class IntervalIso
    extends Iso[IntervalData, Interval]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Interval]) =
      (p.start, p.end)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, end) = p
      Interval(start, end)
    }
    lazy val eTo = new IntervalElem(this)
  }
  // 4) constructor and deconstructor
  class IntervalCompanionAbs extends CompanionDef[IntervalCompanionAbs] with IntervalCompanion {
    def selfType = IntervalCompanionElem
    override def toString = "Interval"
    def apply(p: Rep[IntervalData]): Rep[Interval] =
      isoInterval.to(p)
    def apply(start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      mkInterval(start, end)
  }
  object IntervalMatcher {
    def unapply(p: Rep[Segment]) = unmkInterval(p)
  }
  lazy val Interval: Rep[IntervalCompanionAbs] = new IntervalCompanionAbs
  implicit def proxyIntervalCompanion(p: Rep[IntervalCompanionAbs]): IntervalCompanionAbs = {
    proxyOps[IntervalCompanionAbs](p)
  }

  implicit case object IntervalCompanionElem extends CompanionElem[IntervalCompanionAbs] {
    lazy val tag = weakTypeTag[IntervalCompanionAbs]
    protected def getDefaultRep = Interval
  }

  implicit def proxyInterval(p: Rep[Interval]): Interval =
    proxyOps[Interval](p)

  implicit class ExtendedInterval(p: Rep[Interval]) {
    def toData: Rep[IntervalData] = isoInterval.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    cachedIso[IntervalIso]()

  // 6) smart constructor and deconstructor
  def mkInterval(start: Rep[Int], end: Rep[Int]): Rep[Interval]
  def unmkInterval(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  abstract class AbsSlice
      (start: Rep[Int], length: Rep[Int])
    extends Slice(start, length) with Def[Slice] {
    lazy val selfType = element[Slice]
  }
  // elem for concrete class
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ConcreteElem[SliceData, Slice] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertSegment(x: Rep[Segment]) = Slice(x.start, x.length)
    override def getDefaultRep = Slice(0, 0)
    override lazy val tag = {
      weakTypeTag[Slice]
    }
  }

  // state representation type
  type SliceData = (Int, Int)

  // 3) Iso for concrete class
  class SliceIso
    extends Iso[SliceData, Slice]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Slice]) =
      (p.start, p.length)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, length) = p
      Slice(start, length)
    }
    lazy val eTo = new SliceElem(this)
  }
  // 4) constructor and deconstructor
  class SliceCompanionAbs extends CompanionDef[SliceCompanionAbs] with SliceCompanion {
    def selfType = SliceCompanionElem
    override def toString = "Slice"
    def apply(p: Rep[SliceData]): Rep[Slice] =
      isoSlice.to(p)
    def apply(start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      mkSlice(start, length)
  }
  object SliceMatcher {
    def unapply(p: Rep[Segment]) = unmkSlice(p)
  }
  lazy val Slice: Rep[SliceCompanionAbs] = new SliceCompanionAbs
  implicit def proxySliceCompanion(p: Rep[SliceCompanionAbs]): SliceCompanionAbs = {
    proxyOps[SliceCompanionAbs](p)
  }

  implicit case object SliceCompanionElem extends CompanionElem[SliceCompanionAbs] {
    lazy val tag = weakTypeTag[SliceCompanionAbs]
    protected def getDefaultRep = Slice
  }

  implicit def proxySlice(p: Rep[Slice]): Slice =
    proxyOps[Slice](p)

  implicit class ExtendedSlice(p: Rep[Slice]) {
    def toData: Rep[SliceData] = isoSlice.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    cachedIso[SliceIso]()

  // 6) smart constructor and deconstructor
  def mkSlice(start: Rep[Int], length: Rep[Int]): Rep[Slice]
  def unmkSlice(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  abstract class AbsCentered
      (center: Rep[Int], radius: Rep[Int])
    extends Centered(center, radius) with Def[Centered] {
    lazy val selfType = element[Centered]
  }
  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ConcreteElem[CenteredData, Centered] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertSegment(x: Rep[Segment]) = // Converter is not generated by meta
!!!("Cannot convert from Segment to Centered: missing fields List(center, radius)")
    override def getDefaultRep = Centered(0, 0)
    override lazy val tag = {
      weakTypeTag[Centered]
    }
  }

  // state representation type
  type CenteredData = (Int, Int)

  // 3) Iso for concrete class
  class CenteredIso
    extends Iso[CenteredData, Centered]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Centered]) =
      (p.center, p.radius)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(center, radius) = p
      Centered(center, radius)
    }
    lazy val eTo = new CenteredElem(this)
  }
  // 4) constructor and deconstructor
  class CenteredCompanionAbs extends CompanionDef[CenteredCompanionAbs] with CenteredCompanion {
    def selfType = CenteredCompanionElem
    override def toString = "Centered"
    def apply(p: Rep[CenteredData]): Rep[Centered] =
      isoCentered.to(p)
    def apply(center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      mkCentered(center, radius)
  }
  object CenteredMatcher {
    def unapply(p: Rep[Segment]) = unmkCentered(p)
  }
  lazy val Centered: Rep[CenteredCompanionAbs] = new CenteredCompanionAbs
  implicit def proxyCenteredCompanion(p: Rep[CenteredCompanionAbs]): CenteredCompanionAbs = {
    proxyOps[CenteredCompanionAbs](p)
  }

  implicit case object CenteredCompanionElem extends CompanionElem[CenteredCompanionAbs] {
    lazy val tag = weakTypeTag[CenteredCompanionAbs]
    protected def getDefaultRep = Centered
  }

  implicit def proxyCentered(p: Rep[Centered]): Centered =
    proxyOps[Centered](p)

  implicit class ExtendedCentered(p: Rep[Centered]) {
    def toData: Rep[CenteredData] = isoCentered.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    cachedIso[CenteredIso]()

  // 6) smart constructor and deconstructor
  def mkCentered(center: Rep[Int], radius: Rep[Int]): Rep[Centered]
  def unmkCentered(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  registerModule(Segments_Module)
}

// Seq -----------------------------------
trait SegmentsSeq extends SegmentsDsl with scalan.ScalanSeq {
  self: SegmentsDslSeq =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs {
  }

  case class SeqInterval
      (override val start: Rep[Int], override val end: Rep[Int])
    extends AbsInterval(start, end) {
  }

  def mkInterval
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
    new SeqInterval(start, end)
  def unmkInterval(p: Rep[Segment]) = p match {
    case p: Interval @unchecked =>
      Some((p.start, p.end))
    case _ => None
  }

  case class SeqSlice
      (override val start: Rep[Int], override val length: Rep[Int])
    extends AbsSlice(start, length) {
  }

  def mkSlice
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
    new SeqSlice(start, length)
  def unmkSlice(p: Rep[Segment]) = p match {
    case p: Slice @unchecked =>
      Some((p.start, p.length))
    case _ => None
  }

  case class SeqCentered
      (override val center: Rep[Int], override val radius: Rep[Int])
    extends AbsCentered(center, radius) {
  }

  def mkCentered
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
    new SeqCentered(center, radius)
  def unmkCentered(p: Rep[Segment]) = p match {
    case p: Centered @unchecked =>
      Some((p.center, p.radius))
    case _ => None
  }
}

// Exp -----------------------------------
trait SegmentsExp extends SegmentsDsl with scalan.ScalanExp {
  self: SegmentsDslExp =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs {
  }

  case class ExpInterval
      (override val start: Rep[Int], override val end: Rep[Int])
    extends AbsInterval(start, end)

  object IntervalMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Interval]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Interval]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Interval]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Interval], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Interval], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Interval], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Interval], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Interval], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Interval], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IntervalCompanionMethods {
  }

  def mkInterval
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
    new ExpInterval(start, end)
  def unmkInterval(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntervalElem @unchecked =>
      Some((p.asRep[Interval].start, p.asRep[Interval].end))
    case _ =>
      None
  }

  case class ExpSlice
      (override val start: Rep[Int], override val length: Rep[Int])
    extends AbsSlice(start, length)

  object SliceMethods {
    object end {
      def unapply(d: Def[_]): Option[Rep[Slice]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Slice]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Slice]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Slice], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Slice], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Slice], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Slice], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Slice], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Slice], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SliceCompanionMethods {
  }

  def mkSlice
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
    new ExpSlice(start, length)
  def unmkSlice(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SliceElem @unchecked =>
      Some((p.asRep[Slice].start, p.asRep[Slice].length))
    case _ =>
      None
  }

  case class ExpCentered
      (override val center: Rep[Int], override val radius: Rep[Int])
    extends AbsCentered(center, radius)

  object CenteredMethods {
    object start {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "start" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object end {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Centered], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Centered], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Centered], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Centered], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Centered], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Centered], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CenteredCompanionMethods {
  }

  def mkCentered
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
    new ExpCentered(center, radius)
  def unmkCentered(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CenteredElem @unchecked =>
      Some((p.asRep[Centered].center, p.asRep[Centered].radius))
    case _ =>
      None
  }

  object SegmentMethods {
    object start {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "start" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object end {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Segment], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Segment], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Segment], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Segment], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Segment], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Segment], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SegmentCompanionMethods {
  }
}

object Segments_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRQe24l/Q5oEFam5EIKBQqkdIaEeckDBdVElN4m6BSFTFY13J860s7ObmXFkc+iBQw9wQ1w5VOLYC+KChMQFISEOnBBC4sypgKoe6ImKN7Oz67WbJQYpPox2dt68977ve++t7/2B5qVAL0oXM8wbPlG44ZjnLanqTpsrqkZXAm/AyEWy9+EzX7lX+Jsyj051UXEfy4uSdVElemgPw+TZIQcdVMHcJVIFQir0XMdEaLoBY8RVNOBN6vsDhXuMNDtUqs0OmusF3ugA3Ua5DlpyA+4KoojTYlhKIu37MtEZ0WRfMfvRTjiOwZsaRTOF4prAVEH6EGMpsr9KQmfEAz7yFVq0qe2EOi2wKVE/DISKQ5TA3X7gxds5juEFWuncxIe4CSH6TUcJyvtwsxZi9xbuk20w0eZzkLAkbO/aKDT7QgdVJTkAgi77ITNvhiFCCBR4zSTRGPPTSPhpaH7qDhEUM/oB1oe7IhiOUPTLFRAahuDi1WNcxB5Im3v1j6677z1yan5eXx7qVEoGYREcPZtRDUYK4PH7q5/Ih2/dvZBH1S6qUrnVk0pgV6Ult2zVMOeBMjknBGLRB7XWs9QyUbbAZqokKm7gh5iDJ0vlAujEqEuVNtbvFqw6GdSXVEhi09wwzCV41zLwmrppYcZ27585/8Lv7XfzKD8ZogIuHSh8ETtVqOSQvg9VZijVS8Wymx0nQfzS/T+97zbQ9XzCk3U7mzTgYl7+8nPtp5ffyKNy1xTyJYb7XaBKthnxd0Qr4KqLysEhEdFJ6RAz/XSkVCWP7OEBU5bANPICIFdoLbPlQqJp2TTlnYsJqEUVuh1wUr+0W//L+eHTe7oABVqITqIefEwv/P3r4p4ytalgQCls2/GUQgXo3YSO57OUC8muoD5MikPy+rdfv/3gm+15I96KRfQOZgMS9a0FNAanY+Y2INLlSMXK0MQ7nSDRyyqcE+49mZVe1sa6j9WvRhCdwCfL6w/pjbsfK6Nzbjg5TXZ6N6F9N829M/8ieTzVvrhz5/SDz99/2nRjuUeVj8P6xn/oxbh1TrDXUMJbNGVWxntDJjC+DGwTAdXYSodenb6jUDk2nDqvxQ1tJclsROMsZfuEsFkFl5L26ItFRnhf7c9cFHo9a9ZzMzC06IAC5Dh65o3ViXFTdImm//+QI7BHB/KEyFlumbyId2z5xIbj81TWRRtwkq4CtNlMBE7QOJX07FiWrPMjoEx9YFan4t+YfKnhWmv44j5lBwn0tm9HwFmYL+sZ88WxHQ3Ybz/6bPuVH7/8zYzQqp4NMMF58g8pPTonaarF0eEvTypVheb0vDDJ/gPd6HIzggoAAA=="
}
}

trait SegmentsDsl extends impl.SegmentsAbs {self: SegmentsDsl =>}
trait SegmentsDslSeq extends impl.SegmentsSeq {self: SegmentsDslSeq =>}
trait SegmentsDslExp extends impl.SegmentsExp {self: SegmentsDslExp =>}
