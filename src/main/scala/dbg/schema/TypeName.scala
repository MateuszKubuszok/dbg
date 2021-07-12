package dbg.schema

final case class TypeName[A](fullName: String, shortName: String):

  inline def widen[B >: A]: TypeName[B] = this.asInstanceOf[TypeName[B]]
object TypeName:

  inline def of[A](using typeName: TypeName[A]): TypeName[A] = typeName

  def apply[A](fullName: String): TypeName[A] =
    val segments = fullName.takeWhile(_ != '[').split('.')
    val shortName = segments.init.map(_.head).appended(segments.last).mkString(".")
    TypeName(fullName, shortName)

  inline given derived[A]: TypeName[A] = ${typeNameMacro[A]}

  import scala.quoted._

  def typeNameMacro[A : Type](using Quotes): Expr[TypeName[A]] =
    import quotes.reflect._
    val name = TypeRepr.of[A].show.takeWhile(_ != '[')
    '{ TypeName(${summon[ToExpr[String]].apply(name)}) }
end TypeName
