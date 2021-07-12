package dbg.schema

/** Metadata of a Product type's field. */
sealed trait Field[A]:
  type Type

  def extract(value: A): Type
  val index: Int
  val label: String
  val dbg:   Dbg[Type]
object Field:

  def apply[A, Tpe](index: Int, label: String, dbg: Dbg[Tpe])(e: A => Tpe): Field[A] { type Type = Tpe } =
    Impl(index, label, dbg)(e)

  final case class Impl[A, Tpe](index: Int, label: String, dbg: Dbg[Tpe])(e: A => Tpe) extends Field[A] {
    type Type = Tpe
    def extract(value: A): Type = e(value)
  }
end Field
