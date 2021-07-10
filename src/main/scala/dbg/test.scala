package dbg

case class Yolo(a: Int)

case class Yolo2[B](a: B)

object Tests {

  final case class Inner(
    s: String,
    i: Int,
    c: Char,
    f: Float
  )

  @secure
  final case class InnerSecured(
    s: String,
    i: Int,
    c: Char,
    f: Float
  )

  enum Complex derives Dbg:
    case CaseObject
    case CaseClass(
      s:         String,
      i:         Int,
      c:         Char,
      f:         Float,
      @secure x: String,
      inner:     Inner,
      secure:    InnerSecured
    )
    @secure case Secured(p: String)

  val complex = List(
    Complex.CaseObject,
    Complex.CaseClass(
      s = "test",
      i = 10,
      c = '+',
      f = 1.0f,
      x = "password",
      inner = Inner(
        s = "test",
        i = 10,
        c = '+',
        f = 1.0f
      ),
      secure = InnerSecured(
        s = "test",
        i = 10,
        c = '+',
        f = 1.0f
      )
    ),
    Complex.Secured(
      p = "password"
    )
  )
}

@main def test: Unit = {
  import scala.language.implicitConversions

  given renderer: DbgRenderer = DbgRenderer.Default()

  println(debug"test int ${1}")
  println(debug"test string ${"string"}")
  println(debug"test char ${'+'}")

  println(summon[Dbg[Float]])
  println(debug"test complex: ${Tests.complex}")
}
