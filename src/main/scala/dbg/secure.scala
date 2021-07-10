package dbg

import scala.annotation.StaticAnnotation

class secure extends StaticAnnotation

object secure:

  import scala.deriving._
  import scala.quoted._

  inline def isAnnotated[Tpe]: Boolean = ${mkIsAnnotated[Tpe, secure]}

  def mkIsAnnotated[Tpe: Type, Ann: Type](using q: Quotes): Expr[Boolean] = {
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
  }

  inline def annotatedPositions[ADT]: List[Boolean] = ${mkAnnotatedPositions[ADT, secure]}

  def mkAnnotatedPositions[ADT: Type, Ann: Type](using q: Quotes): Expr[List[Boolean]] =
    import q.reflect._

    val adtType = TypeRepr.of[ADT]
    val annType = TypeRepr.of[Ann]

    val annSymbol = annType.typeSymbol

    adtType.classSymbol.match
      case Some(adtSym) if adtSym.flags.is(Flags.Case) =>
        // TODO: filter out only these from annoteeCls.caseFields
        val fields: List[Expr[Boolean]] = adtSym.primaryConstructor.paramSymss.flatten.map { field =>
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
