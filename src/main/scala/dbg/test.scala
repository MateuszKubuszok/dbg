package dbg

case class Yolo(a: Int)

case class Yolo2[B](a: B)

object Tests {

  enum Complex:// derives Dbg:
    case CaseObject
    case CaseClass(
      s: String,
      i: Int,
      c: Char,
      f: Float
    )

  val complex = List(
    Complex.CaseObject,
    Complex.CaseClass(
      s = "test",
      i = 10,
      c = '+',
      f = 1.0f
    )
  )
}

@main def test: Unit = {
  import scala.language.implicitConversions

  given renderer: DbgRenderer = DbgRenderer.Default()

  println(debug"test int ${1}")
  println(debug"test string ${"string"}")
  println(debug"test char ${'+'}")
  //println(debug"test complex: ${Tests.complex}")
}
