package dbg

import org.scalatest._

import java.util.UUID
import scala.concurrent.duration._

final case class Monomorphic(foo: Long, bar: Float, baz: String)

final case class Polymorphic[A](foo: Long, bar: Float, baz: String, value: A)

@secure
final case class Secured(foo: Long, bar: Float, baz: String)

final case class FieldSecured(foo: Long, bar: Float, baz: String, @secure secured: String)

final case class PolyFieldSecured[A](foo: Long, bar: Float, baz: String, value: A, @secure secured: String)

final case class Nested(mono: Monomorphic, poly: Polymorphic[ADT[Int]], secured: Secured, fieldSecured: FieldSecured)

enum ADT[A]:
  case CaseObject extends ADT[Nothing]
  @secure case SecuredObject extends ADT[Nothing]
  case CaseClass(foo: Long, bar: Float, baz: String, value: A, @secure secured: String)
  @secure case SecureClass(foo: Long, bar: Float, baz: String, value: A, @secure secured: String)

class DbgSpec extends wordspec.AnyWordSpec {

  "Dbg with Default DbgReporter" should {

    given renderer: DbgRenderer = DbgRenderer.Default()

    "correctly render build-in types" in {
      assert(().debug === "()")
      assert(false.debug === "false")
      assert(1.toByte.debug === "1")
      assert(1.toShort.debug === "1")
      assert(1.debug === "1")
      assert(1L.debug === "1L")
      assert(1.0f.debug === "1.0f")
      assert(1.0.debug === "1.0")
      assert(BigInt("1").debug === "1")
      assert(BigDecimal("1.0").debug === "1.0")
      assert('.'.debug === "'.'")
      assert("test".debug === """"test"""")

      assert(1.second.debug === "1 second")
      assert((Duration.Inf: Duration).debug === "Duration.Inf") // TODO: improve

      assert(UUID.fromString("8067cad7-7470-421b-a106-f53176d10fd0").debug === "8067cad7-7470-421b-a106-f53176d10fd0")
      assert(java.time.Duration.ofHours(1).debug === "PT1H")
      assert(java.time.Instant.parse("2021-07-10T17:44:23.895577Z").debug === "2021-07-10T17:44:23.895577Z")
    }

    "correctly render collections" in {
      assert(
        Array("foo", "bar", "baz").debug ===
          """scala.Array(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      assert(
        List("foo", "bar", "baz").debug ===
          """scala.List(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      // TODO: maps, sets, etc
    }

    "correctly derive and render output for monomorphic case class" in {
      assert(
        Monomorphic(1L, 1.0f, "test").debug ===
          """dbg.Monomorphic(
            |  foo = 1L,
            |  bar = 1.0f,
            |  baz = "test"
            |)""".stripMargin
      )
    }

    "correctly derive and render output for polymorphic case class" in {
      assert(
        Polymorphic(1L, 1.0f, "test", '%').debug ===
          """dbg.Polymorphic(
            |  foo = 1L,
            |  bar = 1.0f,
            |  baz = "test",
            |  value = '%'
            |)""".stripMargin
      )
    }

    "correctly derive and render output for secured case class" in {
      assert(Secured(1L, 1.0f, "test").debug === """dbg.Secured[content redacted]""")
    }

    "correctly derive and render output for case class with secured field" in {
      assert(
        FieldSecured(1L, 1.0f, "test", "password").debug ===
          """dbg.FieldSecured(
            |  foo = 1L,
            |  bar = 1.0f,
            |  baz = "test",
            |  secured = java.lang.String[content redacted]
            |)""".stripMargin
      )
      assert(
        PolyFieldSecured(1L, 1.0f, "test", 1, "password").debug ===
          """dbg.PolyFieldSecured(
            |  foo = 1L,
            |  bar = 1.0f,
            |  baz = "test",
            |  value = 1,
            |  secured = java.lang.String[content redacted]
            |)""".stripMargin
      )
    }

    "correctly derive and render output for enum" in {
      assert(ADT.CaseObject.debug === "dbg.ADT case dbg.ADT.CaseObject")
      assert(ADT.SecuredObject.debug === "dbg.ADT case dbg.ADT.SecuredObject[content redacted]")
      assert(
        (ADT.CaseClass(1L, 1.0f, "test", '%', "password"): ADT[Char]).debug ===
          """dbg.ADT case dbg.ADT.CaseClass(
            |  foo = 1L,
            |  bar = 1.0f,
            |  baz = "test",
            |  value = '%',
            |  secured = java.lang.String[content redacted]
            |)""".stripMargin
      )
      assert(
        ADT.SecureClass(1L, 1.0f, "test", '%', "password").debug ===
          "dbg.ADT.SecureClass[content redacted]"
      )
    }

    "correctly derive and render nested structures" in {
      assert(
        Nested(
          mono = Monomorphic(1L, 1.0f, "test"),
          poly = Polymorphic(1L, 1.0f, "test", ADT.CaseClass(1L, 1.0f, "test", 1, "password")),
          secured = Secured(1L, 1.0f, "test"),
          fieldSecured = FieldSecured(1L, 1.0f, "test", "password")
        ).debug === """dbg.Nested(
                      |  mono = dbg.Monomorphic(
                      |    foo = 1L,
                      |    bar = 1.0f,
                      |    baz = "test"
                      |  ),
                      |  poly = dbg.Polymorphic(
                      |    foo = 1L,
                      |    bar = 1.0f,
                      |    baz = "test",
                      |    value = dbg.ADT case dbg.ADT.CaseClass(
                      |      foo = 1L,
                      |      bar = 1.0f,
                      |      baz = "test",
                      |      value = 1,
                      |      secured = java.lang.String[content redacted]
                      |    )
                      |  ),
                      |  secured = dbg.Secured[content redacted],
                      |  fieldSecured = dbg.FieldSecured(
                      |    foo = 1L,
                      |    bar = 1.0f,
                      |    baz = "test",
                      |    secured = java.lang.String[content redacted]
                      |  )
                      |)""".stripMargin
      )
    }
  }
}
