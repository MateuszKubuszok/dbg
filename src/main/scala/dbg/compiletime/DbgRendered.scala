package dbg.compiletime

/** Debug-rendered value used by string interpolation. */
opaque type DbgRendered = String

object DbgRendered:

  def pack(string: String):           DbgRendered = string
  def unpack(seq:  Seq[DbgRendered]): Seq[String] = seq
end DbgRendered
