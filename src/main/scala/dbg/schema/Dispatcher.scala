package dbg.schema

/** Metadata of a sum type elements with a dispatching function. */
final case class Dispatcher[A](subtypes: Array[Subtype[A]])(toOrdinal: A => Int) extends (A => Subtype[A]):

  def apply(value: A): Subtype[A] = subtypes(toOrdinal(value))
