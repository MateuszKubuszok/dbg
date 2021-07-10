package dbg.internal

import dbg.Dbg

trait Field[A] {
  type Type
  def extract(value: A): Type
  val index: Int
  val label: String
  val dbg: Dbg[Type]
}
