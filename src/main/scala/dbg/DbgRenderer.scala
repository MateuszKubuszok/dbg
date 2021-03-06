package dbg

import dbg.schema._

import scala.annotation.implicitNotFound
import scala.util.chaining._

/** Renders value using its Dbg schema to interpret it recurively. */
@implicitNotFound("""DbgRenderer not found.

Don't forget to provide a renderer in your scope e.g. with:
  given renderer: DbgRenderer = DbgRenderer.Default()
""")
trait DbgRenderer:

  def renderOneLine[A](
    typeName: TypeName[A],
    format:   A => String
  )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderLiteral[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderProduct[A](
    typeName: TypeName[A],
    fields:   Fields[A]
  )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderSumType[A](
    typeName:   TypeName[A],
    dispatcher: Dispatcher[A]
  )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderWrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B], skipWrapper: Boolean)(
    value:                          A,
    nesting:                        Int,
    sb:                             StringBuilder
  ): StringBuilder

  def renderSeqLike[A, Elem](
    typeName:   TypeName[A],
    elemDbg:    Dbg[Elem],
    toIterable: A => Iterable[Elem]
  )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderMapLike[K, V](typeName: TypeName[Map[K, V]], keyDbg: Dbg[K], valueDbg: Dbg[V])(
    value:                          Map[K, V],
    nesting:                        Int,
    sb:                             StringBuilder
  ): StringBuilder

  def renderSecured[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder

  final def render[A](dbg: Dbg[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder = dbg match
    case Dbg.OneLine(typeName, format)            => renderOneLine(typeName, format)(value, nesting, sb)
    case Dbg.Literal(typeName)                    => renderLiteral(typeName)(value, nesting, sb)
    case Dbg.Product(typeName, fields)            => renderProduct(typeName, fields.value)(value, nesting, sb)
    case Dbg.SumType(typeName, dispatcher)        => renderSumType(typeName, dispatcher)(value, nesting, sb)
    case Dbg.Wrapper(typeName, unwrap, dbg, skip) => renderWrapper(typeName, unwrap, dbg, skip)(value, nesting, sb)
    case Dbg.SeqLike(typeName, elemDbg, toIt)     => renderSeqLike(typeName, elemDbg, toIt)(value, nesting, sb)
    case Dbg.MapLike(typeName, keyDbg, valueDbg)  => renderMapLike(typeName, keyDbg, valueDbg)(value, nesting, sb)
    case Dbg.Secured(typeName)                    => renderSecured(typeName)(value, nesting, sb)

object DbgRenderer:

  /** Build-in renderer. */
  final case class Default(indent: String = "  ", isShort: Boolean = false) extends DbgRenderer:

    override def renderOneLine[A](
      typeName: TypeName[A],
      format:   A => String
    )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.append(format(value))

    override def renderLiteral[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName)

    override def renderProduct[A](
      typeName: TypeName[A],
      fields:   Fields[A]
    )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName)
      if fields.isEmpty then sb.append("()")
      else {
        val nextNesting = nesting + 1
        sb.append("(\n")
          .appendIntercalate(fields, ",\n") { (field, sb0) =>
            sb0
              .appendIndent(nextNesting)
              .append(field.label)
              .append(" = ")
              .appendDbg(field.dbg, field.extract(value), nextNesting)
          }
          .append("\n")
          .appendIndent(nesting)
          .append(")")
      }

    override def renderSumType[A](
      typeName:   TypeName[A],
      dispatcher: Dispatcher[A]
    )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName).append(" case ")
      val subtype = dispatcher(value)
      sb.appendDbg(subtype.dbg, subtype.cast(value), nesting)

    override def renderWrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B], skipWrapper: Boolean)(
      value:                                   A,
      nesting:                                 Int,
      sb:                                      StringBuilder
    ): StringBuilder =
      if skipWrapper then sb.appendDbg(dbg, unwrap(value), nesting) else
      sb.appendTypeName(typeName).append("(").appendDbg(dbg, unwrap(value), nesting).append(")")

    def renderSeqLike[A, Elem](
      typeName:   TypeName[A],
      elemDbg:    Dbg[Elem],
      toIterable: A => Iterable[Elem]
    )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder =
      val it = toIterable(value)
      if it.isEmpty then sb.appendTypeName(typeName).append("()")
      else
        val nextNesting = nesting + 1
        sb.appendTypeName(typeName)
          .append("(\n")
          .appendIntercalate(it, ",\n") { (elem, sb0) =>
            sb0.appendIndent(nextNesting).appendDbg(elemDbg, elem, nextNesting)
          }
          .append("\n")
          .appendIndent(nesting)
          .append(")")

    override def renderMapLike[K, V](typeName: TypeName[Map[K, V]], keyDbg: Dbg[K], valueDbg: Dbg[V])(
      value:                                   Map[K, V],
      nesting:                                 Int,
      sb:                                      StringBuilder
    ): StringBuilder =
      if value.isEmpty then sb.appendTypeName(typeName).append("()")
      else
        val nextNesting = nesting + 1
        sb.appendTypeName(typeName)
          .append("(\n")
          .appendIntercalate(value, ",\n") { case ((k, v), sb0) =>
            sb0
              .appendIndent(nextNesting)
              .appendDbg(keyDbg, k, nextNesting)
              .append(" -> ")
              .appendDbg(valueDbg, v, nextNesting)
          }
          .append("\n")
          .appendIndent(nesting)
          .append(")")

    override def renderSecured[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName).append("[content redacted]")

    extension (sb: StringBuilder)
      inline def appendTypeName[A](typeName: TypeName[A]): StringBuilder =
        sb.append(if isShort then typeName.shortName else typeName.fullName)

      inline def appendIndent(nesting: Int): StringBuilder =
        var i: Int = nesting
        while i > 0 do
          sb.append(indent)
          i -= 1
        sb

      inline def appendIntercalate[A](as: Iterable[A], separator: String)(
        appendA:                          (A, StringBuilder) => StringBuilder
      ): StringBuilder =
        if as != null || !as.isEmpty then
          as.tail.foldLeft(appendA(as.head, sb)) { (sb0, a) =>
            appendA(a, sb0.append(separator))
          }
        sb

      inline def appendDbg[A](dbg: Dbg[A], value: A, nesting: Int): StringBuilder =
        render(dbg)(value, nesting, sb)
end DbgRenderer
