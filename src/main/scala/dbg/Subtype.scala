package dbg

trait Subtype[A] {
  type Type
  def cast(value: A): Type
  val dbg: Dbg[Type]
}
