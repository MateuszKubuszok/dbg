package dbg

import dbg.internal.{ Field, Subtype, TypeName }

enum Dbg[A]:
  case Primitive(format: A => String)
  case CaseObject(typeName: TypeName[A])
  case CaseClass(typeName: TypeName[A], fields: Array[Field[A]])
  case SealedTrait(typeName: TypeName[A], dispatcher: A => Subtype[A])
  case Wrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B]) extends Dbg[A]
  case SeqLike[A, Elem](typeName: TypeName[Any], elemDbg: Dbg[Elem], toIterable: A => Iterable[Elem]) extends Dbg[A]
  case MapLike[K, V](typeName: TypeName[Any], keyDbg: Dbg[K], valueDbg: Dbg[V]) extends Dbg[Map[K, V]]
object Dbg:

  // primitives

  given Dbg[Unit]       = Primitive(_ => "()")
  given Dbg[Boolean]    = Primitive(_.toString)
  given Dbg[Byte]       = Primitive(_.toString)
  given Dbg[Short]      = Primitive(_.toString)
  given Dbg[Int]        = Primitive(_.toString)
  given Dbg[Long]       = Primitive(_.toString + "L")
  given Dbg[Float]      = Primitive(_.toString + "f")
  given Dbg[Double]     = Primitive(_.toString)
  given Dbg[BigInt]     = Primitive(_.toString)
  given Dbg[BigDecimal] = Primitive(_.toString)
  given Dbg[Char]       = Primitive(c => "'%c'".format(c))
  given Dbg[String]     = Primitive(s => """"%s"""".format(s))

  given Dbg[scala.concurrent.duration.Duration]       = Primitive(_.toString)
  given Dbg[scala.concurrent.duration.FiniteDuration] = Primitive(_.toString)

  given Dbg[java.util.UUID] = Primitive(_.toString)
  given jDuration: Dbg[java.time.Duration] = Primitive(_.toString)
  given Dbg[java.time.Instant] = Primitive(_.toString)

  // TODO: either, option, try, exception(?)

  // collections

  given [A](using A: Dbg[A]): Dbg[Array[A]] = SeqLike[Array[A], A](TypeName("scala.Array"), A, _.toIterable)
  given [A](using A: Dbg[A]): Dbg[List[A]]  = SeqLike[List[A], A](TypeName("scala.List"), A, identity)

  // ADTs

  import scala.compiletime.erasedValue
  import scala.compiletime.summonAll
  import scala.compiletime.summonInline
  import scala.deriving.Mirror

  // T is m.MirroredElemTypes - tuple of Types
  inline def summonDbgs[T <: Tuple]: List[Dbg[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Dbg[t]] :: summonDbgs[ts]

  // T is m.MirroredElemTypes - tuple of Types
  inline def summonTypes[T <: Tuple]: List[TypeName[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[TypeName[t]] :: summonTypes[ts]

  // T is m.MirroredElemLabels - tuple of singleton types describing labels
  inline def summonLabels[T <: Tuple]: List[String] = {
    type ValueOfs = Tuple.Map[T, ValueOf]
    val valueOfs = summonAll[ValueOfs]
    valueOfs.toList.map(_.asInstanceOf[ValueOf].value)
  }

  inline given derived[A](using m: Mirror.Of[A]): Dbg[A] =
    val name = summonInline[TypeName[A]]
    (inline m match {
        // TODO: Mirror.Singleton
      case p: Mirror.ProductOf[A] =>
        dbgProduct(p, name, summonDbgs[p.MirroredElemTypes], summonTypes[p.MirroredElemTypes], summonLabels[p.MirroredElemLabels])
      case s: Mirror.SumOf[A] => dbgSum(s, name, summonDbgs[s.MirroredElemTypes])
    })

  def dbgProduct[A](
    p:      Mirror.ProductOf[A],
    name:   TypeName[A],
    dbgs:   List[Dbg[_]],
    types:  List[TypeName[_]],
    labels: List[String]
  ): Dbg[A] =
    if labels.isEmpty then Dbg.CaseObject(typeName = name)
    else {
      val fields = labels.zipWithIndex.map { case (l, idx) =>
        new Field[A] {
          type Type
          override def extract(value: A): Type = value.asInstanceOf[Product].productElement(idx).asInstanceOf[Type]
          val index = idx
          val label = l
          val dbg   = dbgs(idx).asInstanceOf[Dbg[Type]]
        }
      }.toArray
      Dbg.CaseClass(typeName = name, fields = fields)
    }

  def dbgSum[A](s: Mirror.SumOf[A], name: TypeName[A], dbgs: List[Dbg[_]]): Dbg[A] =
    val subtypes = dbgs.zipWithIndex.map { case (d, idx) =>
      new Subtype[A] {
        type Type
        val dbg = d.asInstanceOf[Dbg[Type]]
      }
    }.toArray
    Dbg.SealedTrait(typeName = name, dispatcher = (a: A) => subtypes(s.ordinal(a)))
end Dbg

// extendsion method

extension [A](value: A)
  def debug(using dbg: Dbg[A], renderer: DbgRenderer): String =
    renderer.render(dbg)(value, nesting = 0, sb = new StringBuilder).toString()

// interpolation

given [A](using Dbg[A], DbgRenderer): Conversion[A, internal.DbgRendered] with
  def apply(value: A): internal.DbgRendered = internal.DbgRendered.pack(value.debug)

extension (sc:        StringContext)
  def debug(rendered: internal.DbgRendered*): String = sc.s(internal.DbgRendered.unpack(rendered): _*)
