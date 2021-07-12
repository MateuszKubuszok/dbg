package dbg

// alias dbg.Dbg to dbg.schema.Dbg

export dbg.schema.Dbg

// extension method

extension [A](value:   A)
  /** Render type using its Dbg schema and renderer */
  def debug(using dbg: Dbg[A], renderer: DbgRenderer): String =
    renderer.render(dbg)(value, nesting = 0, sb = new StringBuilder).toString()

// interpolation

given [A](using Dbg[A], DbgRenderer): Conversion[A, compiletime.DbgRendered] with
  def apply(value: A): compiletime.DbgRendered = compiletime.DbgRendered.pack(value.debug)

extension (sc:        StringContext)
  /** Interpolate all values by rendering them with their Dbg schema and  */
  def debug(rendered: compiletime.DbgRendered*): String = sc.s(compiletime.DbgRendered.unpack(rendered): _*)
