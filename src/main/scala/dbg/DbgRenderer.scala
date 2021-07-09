package dbg

import scala.util.chaining._

trait DbgRenderer:

  def renderPrimitive[A](format: A => String)(value: A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderCaseObject[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderCaseClass[A](
    typeName: TypeName[A],
    fields:   Array[Field[A]]
  )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderSealedTrait[A](
    typeName:   TypeName[A],
    dispatcher: A => Subtype[A]
  )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder

  def renderWrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B])(
    value:                          A,
    nesting:                        Int,
    sb:                             StringBuilder
  ): StringBuilder

  final def render[A](dbg: Dbg[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder = dbg.match {
    case Dbg.Primitive(format)                 => renderPrimitive(format)(value, nesting, sb)
    case Dbg.CaseObject(typeName)              => renderCaseObject(typeName)(value, nesting, sb)
    case Dbg.CaseClass(typeName, fields)       => renderCaseClass(typeName, fields)(value, nesting, sb)
    case Dbg.SealedTrait(typeName, dispatcher) => renderSealedTrait(typeName, dispatcher)(value, nesting, sb)
    case Dbg.Wrapper(typeName, unwrap, dbg)    => renderWrapper(typeName, unwrap, dbg)(value, nesting, sb)
  }

object DbgRenderer:
  final case class Default(indent: String = "  ", isShort: Boolean = false) extends DbgRenderer:

    override def renderPrimitive[A](format: A => String)(value: A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.append(format(value))

    override def renderCaseObject[A](typeName: TypeName[A])(value: A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName)

    override def renderCaseClass[A](
      typeName: TypeName[A],
      fields:   Array[Field[A]]
    )(value:    A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName)
      if fields.isEmpty then sb.append("()")
      else {
        val nextNesting = nesting + 1
        sb.append("(\n")
          .appendIntercalate(fields, ",\n") { (field, sb0) =>
            sb0.appendIndent(nextNesting).appendDbg(field.dbg, field.extract(value), nextNesting)
          }
          .appendIndent(nesting)
          .append(")")
      }

    override def renderSealedTrait[A](
      typeName:   TypeName[A],
      dispatcher: A => Subtype[A]
    )(value:      A, nesting: Int, sb: StringBuilder): StringBuilder =
      sb.appendTypeName(typeName).append(" case ")
      val subtype = dispatcher(value)
      sb.appendDbg(subtype.dbg, subtype.cast(value), nesting)

    override def renderWrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B])(
      value:                                   A,
      nesting:                                 Int,
      sb:                                      StringBuilder
    ): StringBuilder =
      sb.appendTypeName(typeName).append("(").appendDbg(dbg, unwrap(value), nesting).append(")")

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
        if (as != null || !as.isEmpty) {
          as.tail.foldLeft(appendA(as.head, sb)) { (sb0, a) =>
            appendA(a, sb0.append(separator))
          }
        }
        sb

      inline def appendDbg[A](dbg: Dbg[A], value: A, nesting: Int): StringBuilder =
        render(dbg)(value, nesting, sb)
