package dbg.schema

import dbg.Dbg

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

final case class Dispatcher[A](subtypes: Array[Subtype[A]])(toOrdinal: A => Int) extends (A => Subtype[A]):

  def apply(value: A): Subtype[A] = subtypes(toOrdinal(value))
