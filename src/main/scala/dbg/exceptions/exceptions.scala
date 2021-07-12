package dbg.exceptions

import dbg.Dbg

given Dbg[StackTraceElement] = Dbg.OneLine(
  dbg.internal.TypeName.of[StackTraceElement],
  (ste: StackTraceElement) =>
    s"${ste.getModuleName}.${ste.getClassName}.${ste.getMethodName} (${ste.getFileName}:${ste.getLineNumber})"
)

given [E <: Throwable: dbg.internal.TypeName]: Dbg[E] =
  val fields = Array(
    dbg.internal.Field[E, String](0, "message", Dbg.of[String])(_.getMessage),
    dbg.internal.Field[E, Array[StackTraceElement]](1, "stackTrace", Dbg.of[Array[StackTraceElement]])(_.getStackTrace),
    dbg.internal.Field[E, Throwable](2, "cause", Dbg.of[Throwable])(_.getCause)
  )
  Dbg.Product(typeName = dbg.internal.TypeName.of[E], fields = fields)
