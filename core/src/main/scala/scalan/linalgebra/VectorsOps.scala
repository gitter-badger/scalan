package scalan.linalgebra

import scalan.ScalanDsl
import scalan.ScalanSeqImplementation
import scalan.ScalanStaged
import scalan.arrays.PArraysDsl
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scalan.arrays.PArraysDslExp
import scalan.arrays.PArraysDslSeq
import scalan.common.Lazy

trait VectorsOps { scalan: VectorsDsl =>
  trait VectorOps[T] {
    def length: Rep[Int]
    def coords: PA[T]
    implicit def elem: Elem[T]
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T]
    def apply(i: Rep[Int]): Rep[T]
  }
  
  trait VectorCompanionOps extends TypeFamily1[Vector] {
    def defaultOf[T: Elem] = DenseVector.defaultOf[T]
  }
  
  trait DenseVectorOps[T] extends VectorOps[T] {
    def length = coords.length
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = matchVec[T, T](other) {
      dv => dotPA(coords, dv.coords)(n, m, elem)
    } {
      sv =>
        dotPA(coords(sv.nonZeroIndices), sv.nonZeroValues)(n, m, elem)
    }
    def apply(i: Rep[Int]) = coords(i)
  }

  def dotPA[T](xs: PA[T], ys: PA[T])(implicit n: Numeric[T], m: RepMonoid[T], e: Elem[T]) = {
    xs.zip(ys).map {
      case Pair(x, y) =>
        x * y
    }.reduce
  }
  
  trait DenseVectorCompanionOps extends ConcreteClass1[DenseVector] {
    def defaultOf[T: Elem] = Default.defaultVal(DenseVector(element[PArray[T]].defaultRepValue))
  }
  
  trait SparseVectorOps[T] extends VectorOps[T] {
    implicit lazy val n: Numeric[T] = (elem match {
        case `intElement` => implicitly[Numeric[Int]]
        case `doubleElement` => implicitly[Numeric[Double]]
      }).asInstanceOf[Numeric[T]]

    def nonZeroIndices: Arr[Int]
    def nonZeroValues: PA[T]
    def coords: PA[T] = {
      val coords0 = Array.replicate(length, n.zero)
      PArray(coords0.updateMany(nonZeroIndices, nonZeroValues.arr))
    }
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = matchVec[T, T](other) {
      dv => dotPA(dv.coords(nonZeroIndices), nonZeroValues)(n, m, elem)
      // TODO currently doesn't work because there are problems defining toRep for ViewElem
      // dv => dv.dot(this.asInstanceOf[SparseVector[T]])
    } {
      sv => dotSparse(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues)
    }
    def apply(i: Rep[Int]): Rep[T] = coords(i) // TODO better impl
  }

  trait SparseVectorCompanionOps extends ConcreteClass1[SparseVector] {
    def defaultOf[T: Elem] = {
      Default.defaultVal(SparseVector(element[Array[Int]].defaultRepValue, element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
    }
    def apply[T: Elem](coords: PA[T])(implicit n: Numeric[T], o: Overloaded1): Rep[SparseVector[T]] = {
      val indices: Arr[Int] = coords.arr.zip(array_rangeFrom0(coords.length)).
        filter {x => x._1 !== n.zero }.map {x => x._2}
      SparseVector(indices, coords(indices), coords.length)
    }
  }

  def dotSparse[T: Elem](xIndices: Arr[Int], xValues: PA[T], yIndices: Arr[Int], yValues: PA[T])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T]
}

trait VectorsDsl extends ScalanDsl with impl.VectorsAbs with VectorsOps with PArraysDsl {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R]
}

trait VectorsDslSeq extends VectorsDsl with impl.VectorsSeq with PArraysDslSeq with ScalanSeqImplementation {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R] =
    vec match {
      case dv: DenseVector[_] => dense(dv)
      case sv: SparseVector[_] => sparse(sv)
    }

  def dotSparse[T: Elem](xIndices: Arr[Int], xValues: PA[T], yIndices: Arr[Int], yValues: PA[T])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T] = {
    var result = n.zero
    val yMap = yIndices.zip(yValues.arr).toMap
    xIndices.zip(xValues.arr).foldLeft(n.zero) { 
      case (acc, (i, x)) =>
      yMap.get(i) match {
        case Some(y) => acc + x * y
        case None => acc
      }
    }
  }
}

trait VectorsDslExp extends VectorsDsl with impl.VectorsExp with PArraysDslExp with ScalanStaged {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R] =
    vec.elem.asInstanceOf[Elem[_]] match {
      case _: DenseVectorElem[_] => dense(vec.asRep[DenseVector[T]])
      case _: SparseVectorElem[_] => sparse(vec.asRep[SparseVector[T]])
    }
  
  def dotSparse[T: Elem](xIndices: Arr[Int], xValues: PA[T], yIndices: Arr[Int], yValues: PA[T])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T] =
    DotSparse(xIndices, xValues.arr, yIndices, yValues.arr)

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])(implicit val n: Numeric[T], val m: RepMonoid[T], val selfType: Elem[T]) extends Def[T] {
    lazy val self = reifyObject(this)(Lazy(selfType))
    override def mirror(f: Transformer) = DotSparse(f(xIndices), f(xValues), f(yIndices), f(yValues))
    def uniqueOpId = name(selfType)
  }
}