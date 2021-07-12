package dbg

import org.scalatest._

import java.util.UUID
import scala.collection.immutable.{ Queue, SortedMap, SortedSet }
import scala.concurrent.duration._

final case class Monomorphic(foo: Long, bar: Float, baz: String)

final case class Polymorphic[A](foo: Long, bar: Float, baz: String, value: A)

@secure
final case class Secured(foo: Long, bar: Float, baz: String)

final case class FieldSecured(foo: Long, bar: Float, baz: String, @secure secured: String)

final case class PolyFieldSecured[A](foo: Long, bar: Float, baz: String, value: A, @secure secured: String)

final case class Nested(mono: Monomorphic, poly: Polymorphic[ADT[Int]], secured: Secured, fieldSecured: FieldSecured)

final case class Recursive(recur: Option[Recursive]) derives schema.Dbg

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
      // TODO: customize common instances (List, Vector, Set)
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
          """scala.collection.immutable.List(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      assert(
        Vector("foo", "bar", "baz").debug ===
          """scala.collection.immutable.Vector(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      assert(
        Queue("foo", "bar", "baz").debug ===
          """scala.collection.immutable.Queue(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      assert(
        Set("foo", "bar", "baz").debug ===
          """scala.collection.immutable.Set(
            |  "foo",
            |  "bar",
            |  "baz"
            |)""".stripMargin
      )
      assert(
        SortedSet("foo", "bar", "baz").debug ===
          """scala.collection.immutable.SortedSet(
            |  "bar",
            |  "baz",
            |  "foo"
            |)""".stripMargin
      )
      assert(
        Map("key" -> "value", "key2" -> "value2").debug ===
          """scala.collection.immutable.Map(
            |  "key" -> "value",
            |  "key2" -> "value2"
            |)""".stripMargin
      )
      assert(
        SortedMap("key" -> "value", "key2" -> "value2").debug ===
          """scala.collection.immutable.SortedMap(
            |  "key" -> "value",
            |  "key2" -> "value2"
            |)""".stripMargin
      )
    }

    "correctly derive and render common types" in {
      // TODO: customize these instances
      assert(
        (Right(10): Either[String, Int]).debug ===
          """scala.util.Either case scala.util.Right(
            |  value = 10
            |)""".stripMargin
      )
      assert(
        (Option(10): Option[Int]).debug ===
          """scala.Option case scala.Some(
            |  value = 10
            |)""".stripMargin
      )
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
      assert((ADT.CaseObject: ADT.CaseObject.type).debug === "dbg.ADT case dbg.ADT.CaseObject")
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

    "correctly handle recursive types" in {
      // this works only if Recursive uses derives schema.Dbg o_0
      assert(
        Recursive(Some(Recursive(Some(Recursive(None))))).debug ===
          """dbg.Recursive(
            |  recur = scala.Option case scala.Some(
            |    value = dbg.Recursive(
            |      recur = scala.Option case scala.Some(
            |        value = dbg.Recursive(
            |          recur = scala.Option case scala.None
            |        )
            |      )
            |    )
            |  )
            |)""".stripMargin
      )
    }

    "correctly introspect its own Schema" in {
      assert(
        Dbg.schemaOf[Nested] ===
          """dbg.schema.Dbg case dbg.schema.Dbg.Product(
            |  typeName = dbg.schema.TypeName(
            |    fullName = "dbg.Nested",
            |    shortName = "d.Nested"
            |  ),
            |  fields = scala.Array(
            |    dbg.schema.Field(
            |      index = 0,
            |      label = "mono",
            |      dbg = dbg.schema.Dbg case dbg.schema.Dbg.Product(
            |        typeName = dbg.schema.TypeName(
            |          fullName = "dbg.Monomorphic",
            |          shortName = "d.Monomorphic"
            |        ),
            |        fields = scala.Array(
            |          dbg.schema.Field(
            |            index = 0,
            |            label = "foo",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Long",
            |                shortName = "s.Long"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 1,
            |            label = "bar",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Float",
            |                shortName = "s.Float"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 2,
            |            label = "baz",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "java.lang.String",
            |                shortName = "j.l.String"
            |              ),
            |              format = [function]
            |            )
            |          )
            |        )
            |      )
            |    ),
            |    dbg.schema.Field(
            |      index = 1,
            |      label = "poly",
            |      dbg = dbg.schema.Dbg case dbg.schema.Dbg.Product(
            |        typeName = dbg.schema.TypeName(
            |          fullName = "dbg.Polymorphic",
            |          shortName = "d.Polymorphic"
            |        ),
            |        fields = scala.Array(
            |          dbg.schema.Field(
            |            index = 0,
            |            label = "foo",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Long",
            |                shortName = "s.Long"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 1,
            |            label = "bar",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Float",
            |                shortName = "s.Float"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 2,
            |            label = "baz",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "java.lang.String",
            |                shortName = "j.l.String"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 3,
            |            label = "value",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.SumType(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "dbg.ADT",
            |                shortName = "d.ADT"
            |              ),
            |              dispatcher = dbg.schema.Dispatcher(
            |                subtypes = scala.Array(
            |                  dbg.schema.Subtype(
            |                    dbg = dbg.schema.Dbg case dbg.schema.Dbg.Literal(
            |                      typeName = dbg.schema.TypeName(
            |                        fullName = "dbg.ADT.CaseObject",
            |                        shortName = "d.A.CaseObject"
            |                      )
            |                    )
            |                  ),
            |                  dbg.schema.Subtype(
            |                    dbg = dbg.schema.Dbg case dbg.schema.Dbg.Secured(
            |                      typeName = dbg.schema.TypeName(
            |                        fullName = "dbg.ADT.SecuredObject",
            |                        shortName = "d.A.SecuredObject"
            |                      )
            |                    )
            |                  ),
            |                  dbg.schema.Subtype(
            |                    dbg = dbg.schema.Dbg case dbg.schema.Dbg.Product(
            |                      typeName = dbg.schema.TypeName(
            |                        fullName = "dbg.ADT.CaseClass",
            |                        shortName = "d.A.CaseClass"
            |                      ),
            |                      fields = scala.Array(
            |                        dbg.schema.Field(
            |                          index = 0,
            |                          label = "foo",
            |                          dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |                            typeName = dbg.schema.TypeName(
            |                              fullName = "scala.Long",
            |                              shortName = "s.Long"
            |                            ),
            |                            format = [function]
            |                          )
            |                        ),
            |                        dbg.schema.Field(
            |                          index = 1,
            |                          label = "bar",
            |                          dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |                            typeName = dbg.schema.TypeName(
            |                              fullName = "scala.Float",
            |                              shortName = "s.Float"
            |                            ),
            |                            format = [function]
            |                          )
            |                        ),
            |                        dbg.schema.Field(
            |                          index = 2,
            |                          label = "baz",
            |                          dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |                            typeName = dbg.schema.TypeName(
            |                              fullName = "java.lang.String",
            |                              shortName = "j.l.String"
            |                            ),
            |                            format = [function]
            |                          )
            |                        ),
            |                        dbg.schema.Field(
            |                          index = 3,
            |                          label = "value",
            |                          dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |                            typeName = dbg.schema.TypeName(
            |                              fullName = "scala.Int",
            |                              shortName = "s.Int"
            |                            ),
            |                            format = [function]
            |                          )
            |                        ),
            |                        dbg.schema.Field(
            |                          index = 4,
            |                          label = "secured",
            |                          dbg = dbg.schema.Dbg case dbg.schema.Dbg.Secured(
            |                            typeName = dbg.schema.TypeName(
            |                              fullName = "java.lang.String",
            |                              shortName = "j.l.String"
            |                            )
            |                          )
            |                        )
            |                      )
            |                    )
            |                  ),
            |                  dbg.schema.Subtype(
            |                    dbg = dbg.schema.Dbg case dbg.schema.Dbg.Secured(
            |                      typeName = dbg.schema.TypeName(
            |                        fullName = "dbg.ADT.SecureClass",
            |                        shortName = "d.A.SecureClass"
            |                      )
            |                    )
            |                  )
            |                )
            |              )
            |            )
            |          )
            |        )
            |      )
            |    ),
            |    dbg.schema.Field(
            |      index = 2,
            |      label = "secured",
            |      dbg = dbg.schema.Dbg case dbg.schema.Dbg.Secured(
            |        typeName = dbg.schema.TypeName(
            |          fullName = "dbg.Secured",
            |          shortName = "d.Secured"
            |        )
            |      )
            |    ),
            |    dbg.schema.Field(
            |      index = 3,
            |      label = "fieldSecured",
            |      dbg = dbg.schema.Dbg case dbg.schema.Dbg.Product(
            |        typeName = dbg.schema.TypeName(
            |          fullName = "dbg.FieldSecured",
            |          shortName = "d.FieldSecured"
            |        ),
            |        fields = scala.Array(
            |          dbg.schema.Field(
            |            index = 0,
            |            label = "foo",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Long",
            |                shortName = "s.Long"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 1,
            |            label = "bar",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "scala.Float",
            |                shortName = "s.Float"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 2,
            |            label = "baz",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.OneLine(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "java.lang.String",
            |                shortName = "j.l.String"
            |              ),
            |              format = [function]
            |            )
            |          ),
            |          dbg.schema.Field(
            |            index = 3,
            |            label = "secured",
            |            dbg = dbg.schema.Dbg case dbg.schema.Dbg.Secured(
            |              typeName = dbg.schema.TypeName(
            |                fullName = "java.lang.String",
            |                shortName = "j.l.String"
            |              )
            |            )
            |          )
            |        )
            |      )
            |    )
            |  )
            |)""".stripMargin
      )
    }
  }
}
