package dbg.schema

/** Metadata of a sum type element. */
sealed trait Subtype[A]:
  type Type

  def cast(value: A): Type = value.asInstanceOf[Type]
  val dbg: Dbg[Type]
object Subtype:

  def apply[A, Tpe](dbg: Dbg[Tpe]): Subtype[A] { type Type = Tpe } =
    Impl(dbg)

  final case class Impl[A, Tpe](dbg: Dbg[Tpe]) extends Subtype[A] {
    type Type = Tpe
  }
end Subtype
