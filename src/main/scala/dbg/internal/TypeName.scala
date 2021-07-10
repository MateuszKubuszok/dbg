package dbg.internal

final case class TypeName[A](fullName: String, shortName: String)
object TypeName {
  def apply[A](fullName: String): TypeName[A] =
    val segments = fullName.split('.')
    val shortName = segments.view.drop(1).map(_.last).appended(segments.last).mkString(".")
    TypeName(fullName, shortName)

  inline given derived[A]: TypeName[A] = ${typeNameMacro[A]}

  import scala.quoted._

  def typeNameMacro[A : Type](using Quotes): Expr[TypeName[A]] = {
    import quotes.reflect._

    val name = TypeRepr.of[A].show.takeWhile(_ != '[')
    val const = Literal(StringConstant(name)).asExpr.asInstanceOf[Expr[String]]

    '{ TypeName(${const}) }
  }
}
