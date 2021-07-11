package dbg

import dbg.internal.{ Field, Subtype, TypeName }

import scala.annotation.implicitNotFound
import scala.collection.immutable.{ Queue, SortedMap, SortedSet }

@implicitNotFound("""Dbg[$A] not found.

If your type is not a primitive, case class, enum or other supported out-of-the-box
please provide an instance youself with one of:
   given yourType: Dbg[YourType] = Dbg.primitive[YourType](toString)
   given yourType: Dbg[YourType] = Dbg.fromToString[YourType]
   given yourType: Dbg[YourType] = Dbg.secured[YourType]
   given yourType: Dbg[YourType] = Dbg.wrapper[YourType, Inner](unpack)
   given yourColl[A: Dbg]: Dbg[YourColl[A]] = Dbg.seqLike[YourColl, A]
""")
enum Dbg[A]:
  case OneLine(typeName: TypeName[A], format: A => String)
  case Literal(typeName: TypeName[A])
  case Product(typeName: TypeName[A], fields: Array[Field[A]])
  case SumType(typeName: TypeName[A], dispatcher: A => Subtype[A])
  case Wrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B]) extends Dbg[A]
  case SeqLike[A, Elem](typeName: TypeName[A], elemDbg: Dbg[Elem], toIterable: A => Iterable[Elem]) extends Dbg[A]
  case MapLike[K, V](typeName: TypeName[Map[K, V]], keyDbg: Dbg[K], valueDbg: Dbg[V]) extends Dbg[Map[K, V]]
  case Secured(typeName: TypeName[A])

  val typeName: TypeName[A]

  def asSecured: Dbg[A] = Secured(typeName)

  inline def narrow[B <: A]: Dbg[B] = this.asInstanceOf[Dbg[B]]
object Dbg:
  inline def of[A](using dbg: Dbg[A]): Dbg[A] = dbg

  def primitive[A:    TypeName](format: A => String): Dbg[A] = OneLine(TypeName.of[A], format)
  def fromToString[A: TypeName]: Dbg[A] = primitive[A](_.toString)

  def secured[A: TypeName]: Dbg[A] = Secured(TypeName.of[A])

  def wrapper[Outer: TypeName, Inner: Dbg](unwrap: Outer => Inner): Dbg[Outer] =
    Wrapper(TypeName.of[Outer], unwrap, Dbg.of[Inner])

  def seqLike[Coll[A] <: Iterable[A], A: Dbg](using typeName: TypeName[Coll[A]]): Dbg[Coll[A]] =
    SeqLike(typeName, Dbg.of[A], _.toIterable)

  // primitives

  given Dbg[Nothing] = primitive(_ => ???)

  given Dbg[Unit]       = primitive(_ => "()")
  given Dbg[Boolean]    = fromToString
  given Dbg[Byte]       = fromToString
  given Dbg[Short]      = fromToString
  given Dbg[Int]        = fromToString
  given Dbg[Long]       = primitive(_.toString + "L")
  given Dbg[Float]      = primitive(_.toString + "f")
  given Dbg[Double]     = fromToString
  given Dbg[BigInt]     = fromToString
  given Dbg[BigDecimal] = fromToString
  given Dbg[Char]       = primitive(c => "'%c'".format(c))
  given Dbg[String]     = primitive(s => """"%s"""".format(s))

  given Dbg[scala.concurrent.duration.Duration]       = fromToString
  given Dbg[scala.concurrent.duration.FiniteDuration] = fromToString

  given Dbg[java.util.UUID] = fromToString
  given jDuration: Dbg[java.time.Duration] = fromToString
  given Dbg[java.time.Instant] = fromToString

  // collections

  given [A: Dbg]: Dbg[Array[A]]     = SeqLike[Array[A], A](TypeName("scala.Array"), Dbg.of[A], _.toIterable)
  given [A: Dbg]: Dbg[List[A]]      = seqLike[List, A]
  given [A: Dbg]: Dbg[Vector[A]]    = seqLike[Vector, A]
  given [A: Dbg]: Dbg[Queue[A]]     = seqLike[Queue, A]
  given [A: Dbg]: Dbg[Set[A]]       = seqLike[Set, A]
  given [A: Dbg]: Dbg[SortedSet[A]] = seqLike[SortedSet, A]

  given [K: Dbg, V: Dbg]: Dbg[Map[K, V]] = MapLike(TypeName.of[Map[K, V]], Dbg.of[K], Dbg.of[V])
  given [K: Dbg, V: Dbg]: Dbg[SortedMap[K, V]] = MapLike(TypeName.of[SortedMap[K, V]].widen, Dbg.of[K], Dbg.of[V]).narrow

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
  inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ValueOf[t]].value.asInstanceOf[String] :: summonLabels[ts]

  private val singletonRenderer = DbgRenderer.Default()
  inline given singleton[A <: AnyVal, B >: A](using ValueOf[A], Dbg[B]): Dbg[A] =
    given DbgRenderer = singletonRenderer
    val value         = (summonInline[ValueOf[A]].value: B).debug
    Literal(TypeName[A](value))

  inline given derived[A](using m: Mirror.Of[A]): Dbg[A] =
    val name    = summonInline[TypeName[A]]
    val dbgs    = summonDbgs[m.MirroredElemTypes]
    val secured = secure.annotatedPositions[A]
    inline if (secure.isAnnotated[A]) then Secured(name)
    else
      (inline m match {
        case p: Mirror.ProductOf[A] =>
          val types  = summonTypes[p.MirroredElemTypes]
          val labels = summonLabels[p.MirroredElemLabels]
          dbgProduct(p = p, name = name, dbgs = dbgs, secured = secured, types = types, labels = labels)
        case s: Mirror.SumOf[A] =>
          dbgSum(s = s, name = name, dbgs = dbgs, secured = secured)
      })

  def dbgProduct[A](
    p:       Mirror.ProductOf[A],
    name:    TypeName[A],
    dbgs:    List[Dbg[_]],
    secured: List[Boolean],
    types:   List[TypeName[_]],
    labels:  List[String]
  ): Dbg[A] =
    if labels.isEmpty then Literal(typeName = name)
    else {
      val fields = labels.zipWithIndex.map { case (l, idx) =>
        new Field[A] {
          type Type
          override def extract(value: A): Type = value.asInstanceOf[scala.Product].productElement(idx).asInstanceOf[Type]
          val index = idx
          val label = l
          val dbg =
            val d = dbgs(idx).asInstanceOf[Dbg[Type]]
            if secured(idx) then d.asSecured else d
        }
      }.toArray
      Product(typeName = name, fields = fields)
    }

  def dbgSum[A](s: Mirror.SumOf[A], name: TypeName[A], dbgs: List[Dbg[_]], secured: List[Boolean]): Dbg[A] =
    val subtypes = dbgs.zipWithIndex.map { case (d, idx) =>
      new Subtype[A] {
        type Type
        val dbg = {
          val d0 = d.asInstanceOf[Dbg[Type]]
          if secured(idx) then Secured(d0.typeName) else d0
        }
      }
    }.toArray
    SumType(typeName = name, dispatcher = (a: A) => subtypes(s.ordinal(a)))
end Dbg

// extension method

extension [A](value:   A)
  def debug(using dbg: Dbg[A], renderer: DbgRenderer): String =
    renderer.render(dbg)(value, nesting = 0, sb = new StringBuilder).toString()

// interpolation

given [A](using Dbg[A], DbgRenderer): Conversion[A, internal.DbgRendered] with
  def apply(value: A): internal.DbgRendered = internal.DbgRendered.pack(value.debug)

extension (sc:        StringContext)
  def debug(rendered: internal.DbgRendered*): String = sc.s(internal.DbgRendered.unpack(rendered): _*)
