/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.concurrent.util.Duration

/** The trait that represents a `scala.concurrent.Future` with
 *  a bound `scala.concurrent.ExecutionContext` used for
 *  callbacks; intended primarily for use from Java.
 *
 *  There is no reason to use this trait in Scala,
 *  instead you can just import your desired `ExecutionContext`
 *  as an implicit.
 *
 *  Use `scala.concurrent.Future.withExecutionContext()` to
 *  obtain an instance of `FutureWithExecutionContext`.
 *
 *  See `scala.concurrent.Future` for most documentation, all
 *  methods with the same name are equivalent.
 */
sealed trait FutureWithExecutionContext[+T] extends Awaitable[T] {

  protected implicit def executor: ExecutionContext

  def future: Future[T]

  private final def bindFuture[U](f: Future[U]): FutureWithExecutionContext[U] =
    f.withExecutionContext(executor)

  /** See `scala.concurrent.Future.onSuccess` */
  final def onSuccess[U](pf: PartialFunction[T, U]): this.type = {
    future.onSuccess(pf)
    this
  }

  /** See `scala.concurrent.Future.onFailure` */
  final def onFailure[U](callback: PartialFunction[Throwable, U]): this.type = {
    future.onFailure(callback)
    this
  }

  /** See `scala.concurrent.Future.onComplete` */
  final def onComplete[U](func: Either[Throwable, T] => U): this.type = {
    future.onComplete(func)
    this
  }

  final def isCompleted: Boolean =
    future.isCompleted

  final def value: Option[Either[Throwable, T]] =
    future.value

  final def failed: FutureWithExecutionContext[Throwable] =
    bindFuture(future.failed)

  final def foreach[U](f: T => U): Unit =
    future.foreach(f)

  final def map[S](f: T => S): FutureWithExecutionContext[S] =
    bindFuture(future.map(f))

  final def flatMap[S](f: T => FutureWithExecutionContext[S]): FutureWithExecutionContext[S] =
    bindFuture(future.flatMap({ t: T => f(t).future }))

  final def filter(pred: T => Boolean): FutureWithExecutionContext[T] =
    bindFuture(future.filter(pred))

  final def withFilter(p: T => Boolean): FutureWithExecutionContext[T] =
    bindFuture(future.withFilter(p))

  final def collect[S](pf: PartialFunction[T, S]): FutureWithExecutionContext[S] =
    bindFuture(future.collect(pf))

  final def recover[U >: T](pf: PartialFunction[Throwable, U]): FutureWithExecutionContext[U] =
    bindFuture(future.recover(pf))

  final def recoverWith[U >: T](pf: PartialFunction[Throwable, FutureWithExecutionContext[U]]): FutureWithExecutionContext[U] =
    bindFuture(future.recoverWith({
      case t if pf.isDefinedAt(t) => pf(t).future
    }))

  final def zip[U](that: FutureWithExecutionContext[U]): FutureWithExecutionContext[(T, U)] =
    bindFuture(future.zip(that.future))

  final def fallbackTo[U >: T](that: FutureWithExecutionContext[U]): FutureWithExecutionContext[U] =
    bindFuture(future.fallbackTo(that.future))

  final def mapTo[S](implicit tag: ClassTag[S]): FutureWithExecutionContext[S] =
    bindFuture(future.mapTo(tag))

  final def andThen[U](pf: PartialFunction[Either[Throwable, T], U]): FutureWithExecutionContext[T] =
    bindFuture(future.andThen(pf))

  final def either[U >: T](that: FutureWithExecutionContext[U]): FutureWithExecutionContext[U] =
    bindFuture(future.either(that.future))

  final def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    future.ready(atMost)
    this
  }

  @throws(classOf[Exception])
  final def result(atMost: Duration)(implicit permit: CanAwait): T =
    future.result(atMost)
}

/** FutureWithExecutionContext companion object.
 */
object FutureWithExecutionContext {
  private final class ConcreteFutureWithExecutionContext[T](override val future: Future[T], override val executor: ExecutionContext)
    extends FutureWithExecutionContext[T] {
  }

  // this is private; use Future.withExecutionContext() instead
  private[concurrent] def apply[T](f: Future[T], executor: ExecutionContext): FutureWithExecutionContext[T] =
    new ConcreteFutureWithExecutionContext(f, executor)
}
