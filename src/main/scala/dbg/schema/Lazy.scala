package dbg.schema

trait Lazy[A]:

  lazy val value: A

object Lazy:

  def apply[A](thunk: => A): Lazy[A] = new Lazy[A]:
    lazy val value: A = thunk
end Lazy
