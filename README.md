## Debug typeclass

Experimental alternative to Cats' `Show` for Scala 3.

## Why

 * no dependencies
 * derivation
 * indentation
 * long and short types names
 * allows to easily exclude some data from the output with an annotation
 * configurable output (result generated by visitor pattern)
 * `StringBuilder` instead of `String` concatenation

## Usage

Derivation:

```scala
import dbg._

final case class Inner(
  s: String,
  i: Int,
  c: Char,
  f: Float
)

@secure // hide whole case class content
final case class InnerSecured(
  s: String,
  i: Int,
  c: Char,
  f: Float
)

enum Complex derives schema.Dbg: // use build-in derivation mechanics
  case CaseObject
  case CaseClass(
    s:         String,
    i:         Int,
    c:         Char,
    f:         Float,
    @secure x: String, // hide only one field of a case class
    inner:     Inner,
    secure:    InnerSecured
  )
  @secure case Secured(p: String) // hide only one element of a sum type
```

Calling:

```scala
val example = List(
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
```

```scala
import dbg._

// You can:
// - use Default(isShort = true) to use one-letter package names instead of full names
// - use Default(indent = yourIndent) to replace the default 2-spaces as a single indentation
// - provide your own custom DbgRenderer if you want to handle differently collections, maps,
//   products, sum types, etc
given DbgRenderer = DbgRenderer.Default()

println(debug"render example: $example")
//render example: scala.collection.immutable.List(
//  dbg.Tests.Complex case dbg.Tests.Complex.CaseObject,
//  dbg.Tests.Complex case dbg.Tests.Complex.CaseClass(
//    s = "test",
//    i = 10,
//    c = '+',
//    f = 1.0f,
//    x = java.lang.String[content redacted],
//    inner = dbg.Tests.Inner(
//      s = "test",
//      i = 10,
//      c = '+',
//      f = 1.0f
//    ),
//    secure = dbg.Tests.InnerSecured[content redacted]
//  ),
//  dbg.Tests.Complex case dbg.Tests.Complex.Secured[content redacted]
//)
```

More examples of output can be checked in [tests](src/test/scala/dbg/DbgSpec.scala).
