package dbg.internal

import dbg.Dbg

trait Subtype[A] {
  type Type
  def cast(value: A): Type = value.asInstanceOf[Type]
  val dbg: Dbg[Type]
}
