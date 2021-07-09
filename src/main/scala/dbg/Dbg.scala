package dbg

enum Dbg[A]:
  case Primitive(format: A => String)
  case CaseObject(typeName: TypeName[A])
  case CaseClass(typeName: TypeName[A], fields: Array[Field[A]])
  case SealedTrait(typeName: TypeName[A], dispatcher: A => Subtype[A])
  case Wrapper[A, B](typeName: TypeName[A], unwrap: A => B, dbg: Dbg[B]) extends Dbg[A]
object Dbg
end Dbg

// extendsion method

extension [A](value: A)
  def debug(using dbg: Dbg[A], renderer: DbgRenderer): String =
    renderer.render(dbg)(value, nesting = 0, sb = new StringBuilder).toString()

// interpolation

opaque type DbgRendered = String

given [A](using Dbg[A], DbgRenderer): Conversion[A, DbgRendered] with
  def apply(value: A): DbgRendered = value.debug

extension (sc: StringContext)
  def debug(rendered: DbgRendered*): String = sc.s(rendered: _*)
