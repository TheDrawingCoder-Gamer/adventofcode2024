package gay.menkissing.common

import algebra.ring.*
import algebra.instances.all.*
import cats.*
import cats.syntax.all.*
import scala.deriving.*
import scala.quoted.*
import compiletime.*

trait BareVecN[V[_]]:
  def dimensions: Int

trait AllNeighborsImpl[V[_]]:
  def allNeighbors[A](self: V[A])(using ring: Ring[A], eq: Eq[A]): List[V[A]]

trait ProductVecNMixin[V[_] <: Product]
  (using
    mirror: Mirror.ProductOf[V[Any]],
    homo: Macros.TupleIsHomo[mirror.MirroredElemTypes]
  ) extends VecN[V]:

  extension [A](self: V[A])
    def axes: Vector[A] = self.productIterator.toVector.asInstanceOf[Vector[A]]

    override def coord(i: Int): A = self.productElement(i).asInstanceOf[A]

trait VecN[V[_]] extends BareVecN[V]:
  def axis[A](i: Int)(using ring: Ring[A]): V[A]
  // If ls.length != dimensions then error : (
  // def construct[A](ls: Vector[A]): V[A]

  extension [A](self: V[A])
    // equivilant to constructing from axes.map(f)
    // but if want to go fast override this
    def map(f: A => A): V[A]
    def zip(that: V[A])(f: (A, A) => A): V[A]

    // Forall V[_], axes.length will be the same
    // (no Vecs that can be any length)
    def axes: Vector[A]

    def coord(i: Int): A = self.axes(i)

    def withCoord(i: Int, v: A): V[A]

    def offset[D]
      (d: D, n: A)
      (using dirN: IsDirectionN[D] { type Vec = V }, ring: Ring[A]): V[A] =
      self + d.digitalDir * n

    def offset[D]
      (d: D)
      (using dirN: IsDirectionN.VecAux[D, V], ring: Ring[A]): V[A] =
      self + d.digitalDir

    def cardinalNeighbors(using ring: Ring[A]): List[V[A]]

    def allNeighbors(using ring: Ring[A], eq: Eq[A]): List[V[A]]
    final infix def dot(that: V[A])(using ring: Ring[A]): A =
      self.axes.zip(that.axes).map((l, r) => ring.times(l, r)).reduce(ring.plus)

    final infix def +(that: V[A])(using addsg: AdditiveSemigroup[A]): V[A] =
      self.zip(that)(addsg.plus)

    final infix def -(that: V[A])(using addg: AdditiveGroup[A]): V[A] =
      self.zip(that)(addg.minus)

    final infix def *(that: A)(using mulsg: MultiplicativeSemigroup[A]): V[A] =
      self.map(mulsg.times(_, that))

    final infix def taxiDistance
      (that: V[A])
      (using addg: AdditiveGroup[A], signed: Signed[A]): A =
      self.axes.zip(that.axes).map((l, r) => signed.abs(addg.minus(l, r)))
        .reduce(addg.plus)

    final def unary_-(using addg: AdditiveGroup[A]): V[A] =
      self.map(addg.negate)

    final infix def min(that: V[A])(using ord: Order[A]): V[A] =
      self.zip(that)(ord.min)

    final infix def max(that: V[A])(using ord: Order[A]): V[A] =
      self.zip(that)(ord.max)

object VecN:
  def axis[V[_]](using vn: VecN[V])[A](n: Int)(using ring: Ring[A]): V[A] =
    vn.axis(n)

  def countTuple[Elems: Type](using Quotes): Int =
    Type.of[Elems] match
      case '[elem *: elems] => 1 + countTuple[elems]
      case '[EmptyTuple]    => 0

  type CardinalNeighborsFunc[
    V[_]
  ] = [A] => V[A] => (Ring[A]) ?=> List[V[A]]

  private def implCardinalNeighbors[T[_]]
    (body: (T[Any], Ring[Any]) => List[T[Any]]): CardinalNeighborsFunc[T] =
    [A] =>
      (self: T[A]) =>
        (ring: Ring[A]) ?=>
          body(
            self.asInstanceOf[T[Any]],
            ring.asInstanceOf[Ring[Any]]
          ).asInstanceOf[List[T[A]]]

  def defaultCardinalNeighborsMacro[T[_]: Type]
    (using Quotes): Expr[
    CardinalNeighborsFunc[T]
  ] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (x: Expr[Product], ring: Expr[Ring[Any]])
          (using Quotes): Expr[List[T[Any]]] =
          val accessors = (0 until size).map: i =>
            val indexExpr = Expr(i)
            '{ $x.productElement($indexExpr) }
          .toList
          Expr.ofList:
            accessors.map: e =>
              List(
                '{ $ring.minus($e, $ring.one) },
                '{ $ring.plus($e, $ring.one) }
              )
            .zipWithIndex.flatMap: (es, idx) =>
              es.map: e1 =>
                val tp =
                  Expr.ofTupleFromSeq(
                    accessors.zipWithIndex
                      .map((e2, j) => if idx == j then e1 else e2)
                  )
                '{ $m.fromProduct($tp) }

        '{
          implCardinalNeighbors[T]((expr: T[Any], ring: Ring[Any]) =>
            ${ body('expr.asExprOf[Product], 'ring) }
          )
        }

  private def implAllNeighbors[T[_]]
    (body: (T[Any], Ring[Any]) => List[T[Any]]): AllNeighborsSignature[T] =
    [A] =>
      (self: T[A]) =>
        (ring: Ring[A], eq: Eq[A]) ?=>
          body(
            self.asInstanceOf[T[Any]],
            ring.asInstanceOf[Ring[Any]]
          ).asInstanceOf[List[T[A]]]

  type AllNeighborsSignature[
    V[_]
  ] = [A] => V[A] => (Ring[A], Eq[A]) ?=> List[V[A]]
  // Should be marginally more optimized than any
  // runtime implementations, as a lot of the computation
  // is done at comptime
  def defaultAllNeighborsMacro[T[_]: Type]
    (using Quotes): Expr[
    AllNeighborsSignature[T]
  ] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (x: Expr[Product], ring: Expr[Ring[Any]])
          (using Quotes): Expr[List[T[Any]]] =
          val accessors = (0 until size).map: i =>
            val indexExpr = Expr(i)
            '{ $x.productElement($indexExpr) }
          .toList
          Expr.ofList:
            accessors.map: e =>
              List(
                '{ $ring.minus($e, $ring.one) },
                e,
                '{ $ring.plus($e, $ring.one) }
              )
            .sequence.filter:
              // Eq not needed because we testing on the structural expression,
              // not the actual result
              _.zipWithIndex.exists((e, i) => e != accessors(i))
            .map: it =>
              val tp = Expr.ofTupleFromSeq(it)
              '{ $m.fromProduct($tp) }
        '{
          implAllNeighbors[T]((expr: T[Any], ring: Ring[Any]) =>
            ${ body('expr.asExprOf[Product], 'ring) }
          )
        }

  type MapSignature[V[_]] = [A] => V[A] => (A => A) => V[A]

  private def implMap[T[_]]
    (body: (T[Any], Any => Any) => T[Any]): MapSignature[
    T
  ] =
    [A] =>
      (self: T[A]) =>
        (f: A => A) =>
          body(self.asInstanceOf[T[Any]], f.asInstanceOf[Any => Any])
            .asInstanceOf[T[A]]

  def defaultMapMacro[T[_]: Type](using Quotes): Expr[MapSignature[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body(x: Expr[Product], f: Expr[Any => Any]): Expr[T[Any]] =
          val accessors = (0 until size).map: i =>
            val indexExpr = Expr(i)
            '{ $x.productElement($indexExpr) }
          .toList
          val tp = Expr.ofTupleFromSeq(accessors.map(i => '{ $f($i) }))
          '{ $m.fromProduct($tp) }
        '{
          implMap((t: T[Any], f: Any => Any) =>
            ${ body('t.asExprOf[Product], 'f) }
          )
        }

  type ZipSignature[V[_]] = [A] => V[A] => V[A] => ((A, A) => A) => V[A]

  private def implZip[T[_]]
    (body: (T[Any], T[Any], (Any, Any) => Any) => T[Any]): ZipSignature[
    T
  ] =
    [A] =>
      (self: T[A]) =>
        (that: T[A]) =>
          (f: (A, A) => A) =>
            body(
              self.asInstanceOf[T[Any]],
              that.asInstanceOf[T[Any]],
              f.asInstanceOf[(Any, Any) => Any]
            ).asInstanceOf[T[A]]

  def defaultZipMacro[T[_]: Type](using Quotes): Expr[ZipSignature[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (
            x: Expr[Product],
            y: Expr[Product],
            f: Expr[(Any, Any) => Any]
          ): Expr[T[Any]] =
          val accessorsL = (0 until size).map: i =>
            val indexExpr = Expr(i)
            '{ $x.productElement($indexExpr) }
          .toList
          val accessorsR = (0 until size).map: i =>
            val indexExpr = Expr(i)
            '{ $y.productElement($indexExpr) }
          .toList

          val tp =
            Expr.ofTupleFromSeq(
              accessorsL.zip(accessorsR).map((l, r) => '{ $f($l, $r) })
            )
          '{ $m.fromProduct($tp) }
        '{
          implZip((self: T[Any], that: T[Any], f: (Any, Any) => Any) =>
            ${ body('self.asExprOf[Product], 'that.asExprOf[Product], 'f) }
          )
        }

  type AxesSignature[T[_]] = [A] => T[A] => Vector[A]

  private def implAxes[T[_]](body: T[Any] => Vector[Any]): AxesSignature[T] =
    [A] =>
      (self: T[A]) =>
        body(
          self.asInstanceOf[T[Any]]
        ).asInstanceOf[Vector[A]]

  def defaultAxesMacro[T[_]: Type](using Quotes): Expr[AxesSignature[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (
            x: Expr[Product]
          ): Expr[Vector[Any]] =
          val accessors =
            Expr.ofList:
              (0 until size).map: i =>
                val indexExpr = Expr(i)
                '{ $x.productElement($indexExpr) }
              .toList
          '{ $accessors.toVector }
        '{
          implAxes((self: T[Any]) => ${ body('self.asExprOf[Product]) })
        }

  type WithCoordFunc[V[_]] = [A] => V[A] => (Int, A) => V[A]
  private def implWithCoord[T[_]]
    (body: (T[Any], Int, Any) => T[Any]): WithCoordFunc[T] =
    [A] =>
      (self: T[A]) =>
        (idx: Int, value: A) =>
          body(
            self.asInstanceOf[T[Any]],
            idx,
            value.asInstanceOf[Any]
          ).asInstanceOf[T[A]]

  def withCoordMacro[T[_]: Type](using Quotes): Expr[WithCoordFunc[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (
            x: Expr[Product],
            i: Expr[Int],
            v: Expr[Any]
          ): Expr[T[Any]] =
          val accessors = (0 until size).map: idx =>
            val indexExpr = Expr(idx)
            '{ if $indexExpr == $i then $v else $x.productElement($indexExpr) }
          .toList
          val tp = Expr.ofTupleFromSeq(accessors)
          '{ $m.fromProduct($tp) }

        '{
          implWithCoord((self: T[Any], i: Int, v: Any) =>
            ${ body('self.asExprOf[Product], 'i, 'v) }
          )
        }
  type CoordFunc[V[_]] = [A] => V[A] => Int => A
  private def implCoord[T[_]](body: (T[Any], Int) => Any): CoordFunc[T] =
    [A] =>
      (self: T[A]) =>
        (i: Int) => body(self.asInstanceOf[T[Any]], i).asInstanceOf[A]
  def coordMacro[T[_]: Type](using Quotes): Expr[CoordFunc[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (
            self: Expr[Product],
            i: Expr[Int]
          ): Expr[Any] = '{ $self.productElement($i) }

        '{
          implCoord((self: T[Any], i: Int) =>
            ${ body('self.asExprOf[Product], 'i) }
          )
        }

  type AxisFunc[V[_]] = [A] => Int => Ring[A] ?=> V[A]
  private def implAxis[T[_]](body: (Int, Ring[Any]) => T[Any]): AxisFunc[T] =
    [A] =>
      (i: Int) =>
        (ring: Ring[A]) ?=>
          body(i, ring.asInstanceOf[Ring[Any]]).asInstanceOf[T[A]]
  def defaultAxisMacro[T[_]: Type](using Quotes): Expr[AxisFunc[T]] =
    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = countTuple[elementTypes]
        def body
          (
            i: Expr[Int],
            ring: Expr[Ring[Any]]
          ): Expr[T[Any]] =
          val accessors = (0 until size).map: idx =>
            val indexExpr = Expr(idx)
            '{ if $indexExpr == $i then $ring.one else $ring.zero }
          .toList
          val tp = Expr.ofTupleFromSeq(accessors)
          '{ $m.fromProduct($tp) }

        '{
          implAxis((i: Int, ring: Ring[Any]) => ${ body('i, 'ring) })
        }

  def derivedMacro[T[_]: Type](using Quotes): Expr[VecN[T]] =
    val allNeighborsM = defaultAllNeighborsMacro[T]
    val cardinalNeighborsM = defaultCardinalNeighborsMacro[T]
    val mapM = defaultMapMacro[T]
    val zipM = defaultZipMacro[T]
    val axisM = defaultAxisMacro[T]
    val axesM = defaultAxesMacro[T]
    val withCoordM = withCoordMacro[T]
    val coordM = coordMacro[T]

    val ev: Expr[Mirror.ProductOf[T[Any]]] =
      Expr.summon[Mirror.ProductOf[T[Any]]].get

    ev match
      case '{
            $m: Mirror.ProductOf[T[Any]] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val size = Macros.countTupleExpr[elementTypes]

        '{
          new VecN[T]:
            def dimensions: Int = $size

            def axis[A](i: Int)(using ring: Ring[A]): T[A] = $axisM[A](i)
            extension [A](self: T[A])
              def allNeighbors(using ring: Ring[A], eq: Eq[A]): List[T[A]] =
                $allNeighborsM[A](self)
              def cardinalNeighbors(using ring: Ring[A]): List[T[A]] =
                $cardinalNeighborsM[A](self)
              def map(f: A => A): T[A] = $mapM[A](self)(f)
              def zip(that: T[A])(f: (A, A) => A): T[A] =
                $zipM[A](self)(that)(f)
              def axes: Vector[A] = $axesM[A](self)
              def withCoord(i: Int, v: A): T[A] = $withCoordM[A](self)(i, v)
              override def coord(i: Int): A = $coordM[A](self)(i)
        }

  inline def derived[T[_]]
    (using
      m: Mirror.ProductOf[T[Any]],
      homo: Macros.TupleIsHomo[m.MirroredElemTypes]
    ): VecN[T] = ${ derivedMacro[T] }
