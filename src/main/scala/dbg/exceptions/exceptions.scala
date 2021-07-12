package dbg.exceptions

import dbg.Dbg
import dbg.schema._

given Dbg[StackTraceElement] = Dbg.OneLine(
  TypeName.of[StackTraceElement],
  (ste: StackTraceElement) =>
    s"${ste.getModuleName}.${ste.getClassName}.${ste.getMethodName} (${ste.getFileName}:${ste.getLineNumber})"
)

given [E <: Throwable: TypeName]: Dbg[E] =
  val fields = Array(
    Field[E, String](0, "message", Dbg.of[String])(_.getMessage),
    Field[E, Array[StackTraceElement]](1, "stackTrace", Dbg.of[Array[StackTraceElement]])(_.getStackTrace),
    Field[E, Throwable](2, "cause", Dbg.of[Throwable])(_.getCause)
  )
  Dbg.Product(typeName = TypeName.of[E], fields = fields)
