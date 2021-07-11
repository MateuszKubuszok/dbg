package dbg

import scala.annotation.StaticAnnotation

class secure extends StaticAnnotation

object secure:

  // Whether Tpe is annotated
  inline def isAnnotated[Tpe]: Boolean = ${mkIsAnnotated[Tpe, secure]}

  // Whether each element of ADT is annotated.
  inline def annotatedPositions[ADT]: List[Boolean] = ${mkAnnotatedPositions[ADT, secure]}

  import scala.deriving._
  import scala.quoted._

  def mkIsAnnotated[Tpe: Type, Ann: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect._

    val tpeType = TypeRepr.of[Tpe]
    val annType = TypeRepr.of[Ann]

    tpeType.classSymbol.match
      case Some(tpeCls) =>
        tpeCls.annotations.find(_.tpe <:< annType) match {
          case Some(_) => '{ true }
          case None    => '{ false }
        }
      case None =>
        report.throwError(s"Expected class, got ${tpeType.show}")

  def mkAnnotatedPositions[ADT: Type, Ann: Type](using q: Quotes): Expr[List[Boolean]] =
    import q.reflect._

    val adtType = TypeRepr.of[ADT]
    val annType = TypeRepr.of[Ann]

    val annSymbol = annType.typeSymbol

    adtType.classSymbol.match
      case Some(adtSym) if adtSym.flags.is(Flags.Case) =>
        val fields: List[Expr[Boolean]] = adtSym.primaryConstructor.paramSymss.filter { params =>
          params.headOption.fold(true)(_.isTerm) // paramSymss might be List(List(type A), List(val x, val y, val z))
        }.flatten.map { field =>
          field.annotations.find(_.tpe <:< annType) match {
            case Some(_) => '{ true }
            case None    => '{ false }
          }
        }
        Expr.ofList(fields)
      case Some(adtSym) if adtSym.flags.is(Flags.Sealed) =>
        val subclasses: List[Expr[Boolean]] = adtSym.children.map { subsclass =>
          subsclass.annotations.find(_.tpe <:< annType) match {
            case Some(_) => '{ true }
            case None    => '{ false }
          }
        }
        Expr.ofList(subclasses)
      case _ =>
        report.throwError(s"Expected case class or a sealed trait (enum), got ${adtType.show}")
end secure
