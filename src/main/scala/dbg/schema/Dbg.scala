package dbg.schema

import dbg._
import dbg.compiletime.DbgRendered

import scala.annotation.implicitNotFound
import scala.collection.immutable.{ Queue, SortedMap, SortedSet }

/** Schema of type A which can be used by [[dbg.DbgRenderer]] to print debug data. */
@implicitNotFound("""dbg.Dbg[$A] not found.

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
  case Product(typeName: TypeName[A], fields: Lazy[Fields[A]])
  case SumType(typeName: TypeName[A], dispatcher: Dispatcher[A])
  case Wrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B], skipWrapper: Boolean) extends Dbg[A]
  case SeqLike[A, Elem](typeName: TypeName[A], elemDbg: Dbg[Elem], toIterable: A => Iterable[Elem]) extends Dbg[A]
  case MapLike[K, V](typeName: TypeName[Map[K, V]], keyDbg: Dbg[K], valueDbg: Dbg[V]) extends Dbg[Map[K, V]]
  case Secured(typeName: TypeName[A])

  val typeName: TypeName[A]

  def asSecured: Dbg[A] = Secured(typeName)

  inline def narrow[B <: A]: Dbg[B] = this.asInstanceOf[Dbg[B]]
object Dbg:

  /** Summons Dbg instance */
  inline def of[A](using dbg: Dbg[A]): Dbg[A] = dbg

  /** Shows Dbg schema of given type */
  inline def schemaOf[A](using Dbg[A], DbgRenderer): String = of[A].debug

  /** Intended to provide custom implementation of a simple type */
  def primitive[A: TypeName](format: A => String): Dbg[A] = OneLine(TypeName.of[A], format)

  /** Intended to render type using toString */
  def fromToString[A: TypeName]: Dbg[A] = primitive[A](_.toString)

  /** Intended to use with types which should hide their details */
  def secured[A: TypeName]: Dbg[A] = Secured(TypeName.of[A])

  /** Intended to use manually with wrapper types (AnyVals, opaque types, Refined types, @newtypes, ...) */
  def wrapper[Outer: TypeName, Inner: Dbg](unwrap: Outer => Inner): Dbg[Outer] =
    Wrapper(TypeName.of[Outer], unwrap, Dbg.of[Inner], skipWrapper = false)

  /** Intended to provide a collection type */
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

  given [A, B]: Dbg[A => B] = primitive(_ => "[function]")

  // collections

  given [A: Dbg]: Dbg[Array[A]]     = SeqLike[Array[A], A](TypeName("scala.Array"), Dbg.of[A], _.toIterable)
  given [A: Dbg]: Dbg[List[A]]      = seqLike[List, A]
  given [A: Dbg]: Dbg[Vector[A]]    = seqLike[Vector, A]
  given [A: Dbg]: Dbg[Queue[A]]     = seqLike[Queue, A]
  given [A: Dbg]: Dbg[Set[A]]       = seqLike[Set, A]
  given [A: Dbg]: Dbg[SortedSet[A]] = seqLike[SortedSet, A]

  given [K: Dbg, V: Dbg]: Dbg[Map[K, V]] = MapLike(TypeName.of[Map[K, V]], Dbg.of[K], Dbg.of[V])
  given [K: Dbg, V: Dbg]: Dbg[SortedMap[K, V]] =
    MapLike(TypeName.of[SortedMap[K, V]].widen, Dbg.of[K], Dbg.of[V]).narrow

  // ADTs and rerivation

  import scala.compiletime.erasedValue
  import scala.compiletime.summonAll
  import scala.compiletime.summonInline
  import scala.deriving.Mirror

  // T is m.MirroredElemTypes - tuple of Types
  inline def summonDbgs[T <: Tuple]: List[Lazy[Dbg[_]]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => Lazy(summonInline[Dbg[t]]) :: summonDbgs[ts]

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
  inline given singleton[A <: AnyVal: ValueOf, B >: A: Dbg]: Dbg[A] =
    given DbgRenderer = singletonRenderer
    val value         = (summonInline[ValueOf[A]].value: B).debug
    Literal(TypeName[A](value))

  // TODO: handle recursive types
  inline given derived[A](using m: Mirror.Of[A]): Dbg[A] =
    val name = summonInline[TypeName[A]]
    inline if (secure.isAnnotated[A]) then Secured(name)
    else
      inline m match
        case p: Mirror.ProductOf[A] =>
          val dbgs    = summonDbgs[p.MirroredElemTypes]
          val secured = secure.annotatedPositions[p.MirroredType]
          val types   = summonTypes[p.MirroredElemTypes]
          val labels  = summonLabels[p.MirroredElemLabels]
          dbgProduct(p = p, name = name, dbgs = dbgs, secured = secured, types = types, labels = labels)
        case s: Mirror.SumOf[A] =>
          val dbgs    = summonDbgs[s.MirroredElemTypes]
          val secured = secure.annotatedPositions[s.MirroredMonoType]
          dbgSum(s = s, name = name, dbgs = dbgs, secured = secured)

  def dbgProduct[A](
    p:       Mirror.ProductOf[A],
    name:    TypeName[A],
    dbgs:    List[Lazy[Dbg[_]]],
    secured: List[Boolean],
    types:   List[TypeName[_]],
    labels:  List[String]
  ): Dbg[A] =
    // case objects in ADTs are treated as ADTs companion... wtf?!? o_0
    assert(dbgs.sizeCompare(secured) == 0 || dbgs.isEmpty,
           s"$name: Size of Dbg($dbgs) is different than Secured($secured)"
    )
    assert(dbgs.sizeCompare(types) == 0, s"$name: Size of Dbg($dbgs) is different than Types($types)")
    assert(dbgs.sizeCompare(labels) == 0, s"$name: Size of Dbg($dbgs) is different than Labels($labels)")
    if labels.isEmpty then Literal(typeName = name)
    else
      val fields = Lazy(
        labels.zipWithIndex.map { case (label, index) =>
          type Type
          Field[A, Type](
            index = index,
            label = label,
            dbg =
              val d = dbgs(index).value.asInstanceOf[Dbg[Type]]
              if secured(index) then d.asSecured else d
          )(_.asInstanceOf[scala.Product].productElement(index).asInstanceOf[Type])
        }.toArray
      )
      Product(typeName = name, fields = fields)

  def dbgSum[A](s: Mirror.SumOf[A], name: TypeName[A], dbgs: List[Lazy[Dbg[_]]], secured: List[Boolean]): Dbg[A] =
    assert(dbgs.sizeCompare(secured) == 0, s"$name: Size of Dbg($dbgs) is different than Secured($secured)")
    // does subtypes need to be Lazy?
    val subtypes = dbgs.zipWithIndex.map { case (d, index) =>
      type Type
      Subtype[A, Type](
        dbg =
          val d0: Dbg[Type] = d.value.asInstanceOf[Dbg[Type]]
          if secured(index) then d0.asSecured else d0
      )
    }.toArray
    SumType(typeName = name, dispatcher = Dispatcher(subtypes)(s.ordinal))

  // instances for Dbg types which cannot be actually derived (because of dependant-types)
  // to save some memory these instances are derived only once (Dbg params are NOT use anywhere in the derivation)

  private val reusableFieldDbg =
    Product[Field[String]](
      typeName = TypeName.of[Field[String]],
      fields = Lazy(
        Array(
          Field[Field[String], Int](0, "index", Dbg.of[Int])(_.index),
          Field[Field[String], String](1, "label", Dbg.of[String])(_.label),
          Field[Field[String], Dbg[String]](2, "dbg", reusabeDbgDbg)(_.dbg.asInstanceOf[Dbg[String]])
        )
      )
    )
  private val reusableSubTypeDbg =
    Product[Subtype[String]](
      typeName = TypeName.of[Subtype[String]],
      fields = Lazy(
        Array(
          Field[Subtype[String], Dbg[String]](0, "dbg", reusabeDbgDbg)(_.dbg.asInstanceOf[Dbg[String]])
        )
      )
    )
  private val reusableDispatcherDbg =
    Product[Dispatcher[String]](
      typeName = TypeName.of[Dispatcher[String]],
      fields = Lazy(
        Array(
          Field[Dispatcher[String], Array[Subtype[String]]](0, "subtypes", of[Array[Subtype[String]]])(_.subtypes)
        )
      )
    )

  private val reusableWrapperDbg = derived[Dbg.Wrapper[String, String]]
  private val reusableSeqLikeDbg = derived[SeqLike[String, String]]
  private val reusableMapLikeDbg = derived[MapLike[String, String]]
  private val reusabeDbgDbg: Dbg[Dbg[String]] = derived[Dbg[String]]

  given [A: Dbg]: Dbg[Lazy[A]] = Wrapper[Lazy[A], A](TypeName.of[Lazy[A]], _.value, Dbg.of[A], skipWrapper = true)
  given [A]: Dbg[Field[A]]      = reusableFieldDbg.asInstanceOf[Dbg[Field[A]]]
  given [A]: Dbg[Subtype[A]]    = reusableSubTypeDbg.asInstanceOf[Dbg[Subtype[A]]]
  given [A]: Dbg[Dispatcher[A]] = reusableDispatcherDbg.asInstanceOf[Dbg[Dispatcher[A]]]

  given [A, B]: Dbg[Wrapper[A, B]] = reusableWrapperDbg.asInstanceOf[Dbg[Wrapper[A, B]]]
  given [A, B]: Dbg[SeqLike[A, B]] = reusableSeqLikeDbg.asInstanceOf[Dbg[SeqLike[A, B]]]
  given [A, B]: Dbg[MapLike[A, B]] = reusableMapLikeDbg.asInstanceOf[Dbg[MapLike[A, B]]]

  given [A]: Dbg[Dbg[A]] = reusabeDbgDbg.asInstanceOf[Dbg[Dbg[A]]]
end Dbg
