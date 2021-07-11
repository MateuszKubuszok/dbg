package dbg.exceptions

import dbg.Dbg

given Dbg[StackTraceElement] = Dbg.OneLine(
  dbg.internal.TypeName.of[StackTraceElement],
  (ste: StackTraceElement) =>
    s"${ste.getModuleName}.${ste.getClassName}.${ste.getMethodName} (${ste.getFileName}:${ste.getLineNumber})"
)

given [E <: Throwable: dbg.internal.TypeName]: Dbg[E] =
  val fields = Array(
    new dbg.internal.Field[E]:
      type Type = String

      def extract(value: E): Type = value.getMessage
      val index: Int       = 0
      val label: String    = "message"
      val dbg:   Dbg[Type] = Dbg.of[String]
    ,
    new dbg.internal.Field[E]:
      type Type = Array[StackTraceElement]

      def extract(value: E): Type = value.getStackTrace
      val index: Int       = 1
      val label: String    = "stackTrace"
      val dbg:   Dbg[Type] = Dbg.of[Array[StackTraceElement]]
    ,
    new dbg.internal.Field[E]:
      type Type = Throwable

      def extract(value: E): Type = value.getCause
      val index: Int       = 2
      val label: String    = "cause"
      val dbg:   Dbg[Type] = Dbg.of[Throwable]
  )
  Dbg.Product(typeName = dbg.internal.TypeName.of[E], fields = fields)
