package scalan.collections

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map;

trait Maps extends  Base  { self: Scalan =>
  type PM[K, V] = Rep[PMap[K, V]]

  trait PMap[K, V] {
    implicit def elemKey: Elem[K]
    implicit def elemValue: Elem[V]

    def union(that: PM[K, V]): PM[K, V]
    def difference(that: PM[K, V]): PM[K, V]
    def join[V2:Elem](that: PM[K, V2]): PM[K, (V, V2)]
    def reduce(that: PM[K, V], f:Rep[((V, V))=>V]): PM[K, V]
    def isEmpty: Rep[Boolean] = (size === 0)
    def contains(k: Rep[K]): Rep[Boolean]
    def apply(key: Rep[K]): Rep[V]
    def applyIf[T:Elem](key: Rep[K], exists: Rep[V]=>Rep[T], otherwise: UnitRep=>Rep[T]): Rep[T]
    def update(key: Rep[K], value: Rep[V]): Rep[Unit]
    def mapValues[T:Elem](f: Rep[V] => Rep[T]): PM[K, T]
    def keys: Arr[K]
    def values: Arr[V]
    def toArray: Arr[(K,V)]
    def size: Rep[Int]
  }

  object PMap {
    def empty[K: Elem, V: Elem] = emptyMap[K, V]
    def create[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]) = createMap[K, V](count,f)
    def make[K: Elem, V: Elem](name:Rep[String]) = makeMap[K, V](name)
    def fromArray[K: Elem, V: Elem](arr: Arr[(K, V)]) = mapFromArray(arr)
  }

  class PMapElem[K, V](val eKey: Elem[K], val eValue: Elem[V]) extends Element[PMap[K, V]] {
    override def isEntityType = eKey.isEntityType || eValue.isEntityType

    lazy val tag = {
      implicit val kt = eKey.tag
      implicit val vt = eValue.tag
      weakTypeTag[PMap[K, V]]
    }

    protected def getDefaultRep = emptyMap[K, V](eKey, eValue)
  }

  implicit def mapElement[K, V](implicit eKey: Elem[K], eValue: Elem[V]): PMapElem[K, V] = new PMapElem(eKey, eValue)
  def extendPMapElement[K, V](implicit elem: Elem[PMap[K, V]]) = elem.asInstanceOf[PMapElem[K, V]]

  implicit def resolvePMap[K: Elem, V: Elem](map: PM[K, V]): PMap[K, V]

  def emptyMap[K: Elem, V: Elem]: PM[K, V]
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]): PM[K, V]
  def createMap[K: Elem, V: Elem](count: Rep[Int], f: Rep[Int=>(K,V)]): PM[K, V]
  def makeMap[K: Elem, V: Elem](name: Rep[String]): PM[K, V]
}

trait MapsSeq extends Maps { self: ScalanSeq =>
  implicit class SeqMap[K, V](val impl: Map[K, V])(implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends PMap[K, V] {
    private def implOf[A,B](that: PMap[A, B]) = that match {
      case m: SeqMap[A, B] => m.impl
      case _ => !!!(s"$that implements PMap in sequential context but is not SeqMap")
    }

    def union(that: PM[K, V]): PM[K, V]  = impl ++ implOf(that)
    def difference(that: PM[K, V]): PM[K, V]  = impl -- implOf(that).keys
    def join[V2:Elem](that: PM[K, V2]): PM[K, (V, V2)] = {
      val res = Map.empty[K, (V, V2)]
      val left = impl
      val right = implOf(that)
      for ((k,v) <- left) {
        if (right.contains(k)) res.update(k, (v, right(k)))
      }
      res
    }      
    def reduce(that: PM[K, V], f:Rep[((V,V))=>V]): PM[K, V] = {
      val res = Map.empty[K, V]
      val left = impl
      val right = implOf(that)
      for ((k,v) <- left) {
        res.update(k, if (right.contains(k)) f((v, right(k))) else v)
      }
      for ((k,v) <- right) {
        if (!left.contains(k)) res.update(k, v)
      }
      res
    }      
    def contains(key: Rep[K]): Rep[Boolean] = impl.contains(key)
    def apply(key: Rep[K]): Rep[V] = impl(key)
    def applyIf[T:Elem](key: Rep[K], exists:(Rep[V]=>Rep[T]), otherwise: UnitRep=>Rep[T]): Rep[T] = {
      if (impl.contains(key)) exists(impl(key)) else otherwise(())
    }
    def update(key: Rep[K], value: Rep[V]): Rep[Unit] = { impl.update(key, value) ; () }
    def keys: Arr[K] = impl.keys.toArray(elemKey.classTag)
    def values: Arr[V] = impl.values.toArray(elemValue.classTag)
    def toArray: Arr[(K, V)] = impl.toArray
    def size: Rep[Int] = impl.size
    def mapValues[T:Elem](f: Rep[V] => Rep[T]): PM[K, T] = {
      val res = Map.empty[K, T]
      for ((k,v) <- impl) {
         res.update(k, f(v))
      }
      res
    }
  }

  implicit def resolvePMap[K: Elem, V: Elem](map: PM[K, V]): PMap[K, V] = map

  def emptyMap[K: Elem, V: Elem]: PM[K, V] = Map.empty[K, V]
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]): PM[K, V] = Map(arr: _*)
  def createMap[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]): PM[K, V] = {
    val map = Map.empty[K, V]
    for (i <- 0 until count) {
      val p = f(i)
      map.update(p._1, p._2)
    }
    map
  }
  def makeMap[K: Elem, V: Elem](name: Rep[String]): PM[K, V] = {
    Map.empty[K, V]
  }
}


trait MapsExp extends Maps { self: ScalanExp =>
  abstract class PMapDef[K, V](implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends PMap[K, V] with Def[PMap[K, V]] {
    def selfType = element[PMap[K, V]]
    lazy val uniqueOpId = name(elemKey, elemValue)

    def union(that: PM[K, V]): PM[K, V] = MapUnion(this, that)
    def difference(that: PM[K, V]): PM[K, V] = MapDifference(this, that)
    def join[V2:Elem](that: PM[K, V2]): PM[K, (V, V2)] = MapJoin(this, that)
    def reduce(that: PM[K, V], f:Rep[((V,V))=>V]): PM[K, V] = MapReduce(this, that, f)
    def contains(key: Rep[K]): Rep[Boolean] = MapContains(this, key)
    def apply(key: Rep[K]): Rep[V] = MapApply(this, key)
    def applyIf[T:Elem](key: Rep[K], exists:Rep[V]=>Rep[T], otherwise: UnitRep=>Rep[T]): Rep[T] = MapApplyIf(this, key, exists, otherwise)
    def update(key: Rep[K], value: Rep[V]): Rep[Unit] = MapUpdate(this, key, value)
    def size: Rep[Int] = MapSize(this)
    def keys: Arr[K] = MapKeys(this)
    def values: Arr[V] = MapValues(this)
    def toArray: Arr[(K, V)] = MapToArray(this)
    def mapValues[T:Elem](f: Rep[V] => Rep[T]): PM[K, T] = MapTransformValues[K,V,T](this, f)
  }

//  def emptyMap[K: Elem, V: Elem]: PM[K, V] = EmptyMap[K, V]()
  def emptyMap[K: Elem, V: Elem]: PM[K, V] = MapUsingFunc(0, fun { i => (element[K].defaultRepValue, element[V].defaultRepValue) })
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]) = MapFromArray(arr)
  def createMap[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]) = MapUsingFunc(count, f)
  def makeMap[K: Elem, V: Elem](name: Rep[String]): PM[K, V] = MakeMap[K,V](name)


  case class AppendMultiMap[K:Elem, V:Elem](map: Rep[PMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V])
    extends PMapDef[K,ArrayBuffer[V]]
  {
    override def mirror(t: Transformer) = AppendMultiMap(t(map), t(key), t(value))
  }

  case class EmptyMap[K: Elem, V: Elem]() extends PMapDef[K, V] {
    override def equals(other:Any) = {
      other match {
        case that:EmptyMap[_,_] => (this.selfType equals that.selfType)
        case _ => false
      }
    }
    override def mirror(t:Transformer) = EmptyMap[K,V]()
  }

  case class MapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = MapFromArray(t(arr))
  }

  case class MapUsingFunc[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = MapUsingFunc(t(count), t(f))
  }

  case class MakeMap[K: Elem, V: Elem](ctx: Rep[String]) extends PMapDef[K, V] {
    /*
    override def equals(other:Any) = {
      other match {
        case that:MakeMap[_,_] => (this.selfType equals that.selfType) && (this.ctx equals that.ctx) && super.equals(that)
        case _ => false
      }
    }
    */
    override def mirror(t: Transformer) = MakeMap[K,V](t(ctx))
  }

  case class MapUnion[K: Elem, V: Elem](left: PM[K, V], right: PM[K, V]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = MapUnion(t(left), t(right))
  }

  case class MapDifference[K: Elem, V: Elem](left: PM[K, V], right: PM[K, V]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = MapDifference(t(left), t(right))
  }

  case class MapJoin[K: Elem, V1: Elem, V2: Elem](left: PM[K, V1], right: PM[K, V2]) extends PMapDef[K, (V1, V2)] {
    override def mirror(t: Transformer) = MapJoin(t(left), t(right))
  }

  case class MapReduce[K: Elem, V: Elem](left: PM[K, V], right: PM[K, V], f:Rep[((V, V))=>V]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = MapReduce(t(left), t(right), t(f))
  }

  case class MapContains[K: Elem, V: Elem](map: PM[K, V], key: Rep[K]) extends BaseDef[Boolean] {
    override def mirror(t: Transformer) = MapContains(t(map), t(key))
    def uniqueOpId = name(selfType)
  }

  case class MapApply[K: Elem, V: Elem](map: PM[K, V], key: Rep[K]) extends BaseDef[V] {
    override def mirror(t: Transformer) = MapApply(t(map), t(key))
    def uniqueOpId = name(selfType)
  }

  case class MapApplyIf[K: Elem, V: Elem, T: Elem](map: PM[K, V], key: Rep[K], exists:Rep[V=>T], otherwise: Rep[Unit=>T]) extends BaseDef[T] {
    override def mirror(t: Transformer) = MapApplyIf(t(map), t(key), t(exists), t(otherwise))
    def uniqueOpId = name(selfType)
  }

  case class MapUpdate[K: Elem, V: Elem](map: PM[K, V], key: Rep[K], value: Rep[V]) extends BaseDef[Unit] {
    override def mirror(t: Transformer) = MapUpdate(t(map), t(key), t(value))
    def uniqueOpId = name(selfType)
  }

  case class MapSize[K: Elem, V: Elem](map: PM[K, V]) extends BaseDef[Int] {
    override def mirror(t: Transformer) = MapSize(t(map))
    def uniqueOpId = name(selfType)
  }

  case class MapToArray[K: Elem, V: Elem](map: PM[K, V]) extends Def[Array[(K, V)]] {
    def selfType = element[Array[(K, V)]]
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = MapToArray(t(map))
  }

  case class MapKeys[K: Elem, V: Elem](map: PM[K, V]) extends Def[Array[K]] {
    def selfType = element[Array[K]]
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = MapKeys(t(map))
  }

  case class MapValues[K: Elem, V: Elem](map: PM[K, V]) extends Def[Array[V]] {
    def selfType = element[Array[V]]
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = MapValues(t(map))
  }

  case class MapTransformValues[K: Elem, V: Elem, T: Elem](map: PM[K, V], f:Rep[V=>T]) extends PMapDef[K, T] {
    override def mirror(t: Transformer) = MapTransformValues(t(map), t(f))
  }

  case class VarPM[K: Elem, V: Elem](map: PM[K, V]) extends PMapDef[K, V] {
    override def mirror(t: Transformer) = VarPM(t(map))
  }

  implicit def resolvePMap[K: Elem, V: Elem](sym: PM[K, V]): PMap[K, V] = sym match  {
    case Def(d: PMapDef[_, _]) => d.asInstanceOf[PMap[K, V]]
    case s: Exp[_] => {
      val pmElem = s.elem.asInstanceOf[PMapElem[K, V]]
      VarPM(sym)(pmElem.eKey, pmElem.eValue)
    }
    case _ => ???("cannot resolve ReifiableObject for symbol:", sym)
  }
}
/*
trait MapViewsExp extends MapsExp with ViewsExp with BaseExp { self: ScalanExp =>
  case class ViewMap[K1, V1, K2, V2](source: PM[K1, V1])(implicit iso1: Iso[K1, K2], iso2: Iso[V1, V2]) extends View2[K1, V1, K2, V2, PMap] {
    lazy val iso = mapIso(iso1, iso2)
    def copy(source: PM[K1, V1]) = ViewMap(source)
    override def toString = s"ViewMap[${iso1.eTo.name},${iso2.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewMap[_, _, _, _] => source == v.source && iso1.eTo == v.iso1.eTo && iso2.eTo == v.iso2.eTo
      case _ => false
    }
  }

  override def hasViews(s: Exp[_]): Boolean = s match {
    case Def(ViewMap(_)) => true
    case s => super.hasViews(s)
  }

  override def eliminateViews(s: Exp[_]): (Exp[_], Iso[_, _]) =
    s match {
      case Def(view: ViewMap[_, _, _, _]) =>
        (view.source, mapIso(view.iso1, view.iso2))
      case s =>
        super.eliminateViews(s)
    }

  def mapIso[K1, V1, K2, V2](iso1: Iso[K1, K2], iso2: Iso[V1, V2]): Iso[PMap[K1, V1], PMap[K2, V2]] = {
    implicit val k1 = iso1.eFrom
    implicit val k2 = iso1.eTo
    implicit val v1 = iso2.eFrom
    implicit val v2 = iso2.eTo
    new Iso[PMap[K1, V1], PMap[K2, V2]] {
      lazy val eTo = element[PMap[K2, V2]]
      def from(x: PM[K2, V2]) = PMap.fromArray[K1, V1](x.keys.map(iso1.from _) zip x.values.map(iso2.from _))
      def to(x: PM[K1, V1]) = PMap.fromArray[K2, V2](x.keys.map(iso1.to _) zip x.values.map(iso2.to _))
      lazy val tag = {
        implicit val tK = iso1.tag
        implicit val tV = iso2.tag
        typeTag[PMap[K2, V2]]
      }
      lazy val defaultRepTo = Default.defaultVal(emptyMap[K2, V2](k2, v2))
    }
  }

  val HasViewArg = HasArg {
    case Def(_: ViewMap[_, _, _, _]) => true
    case _ => false
  }


  def liftViewFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case _ => None
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case HasViewArg(_) => liftViewFromArgs(d) match {
      case Some(s) => s
      case _ => super.rewriteDef(d)
    }
    case view1@ViewMap(Def(view2@ViewMap(map))) =>
      val compIso1 = composeIso(view2.iso1, view1.iso1)
      val compIso2 = composeIso(view2.iso2, view1.iso2)
      implicit val kAB = compIso1.eTo
      implicit val vAB = compIso2.eTo
      ViewMap(map)(compIso1, compIso2)
    case _ =>
      super.rewriteDef(d)
  }
}
*/

