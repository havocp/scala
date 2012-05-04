/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.TimeUnit.{ NANOSECONDS, MILLISECONDS }
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.concurrent.{Awaitable, ExecutionContext, resolveEither, resolver, blocking, CanAwait, TimeoutException}
//import scala.util.continuations._
import scala.concurrent.util.Duration
import scala.util
import scala.annotation.tailrec
//import scala.concurrent.NonDeterministic



private[concurrent] trait Promise[T] extends scala.concurrent.Promise[T] with Future[T] {
  def future: this.type = this
}


object Promise {
  /** Default promise implementation.
   */
  class DefaultPromise[T] extends AbstractPromise with Promise[T] { self =>
    updater.set(this, Nil) // Start at "No callbacks" //FIXME switch to Unsafe instead of ARFU

    protected final def tryAwait(atMost: Duration): Boolean = {
      @tailrec
      def awaitUnsafe(waitTimeNanos: Long): Boolean = {
        if (value.isEmpty && waitTimeNanos > 0) {
          val ms = NANOSECONDS.toMillis(waitTimeNanos)
          val ns = (waitTimeNanos % 1000000l).toInt // as per object.wait spec
          val start = System.nanoTime()
          try {
            synchronized {
              while (!isCompleted) wait(ms, ns)
            }
          } catch {
            case e: InterruptedException =>
          }

          awaitUnsafe(waitTimeNanos - (System.nanoTime() - start))
        } else
          isCompleted
      }
      //FIXME do not do this if there'll be no waiting
      blocking(Future.body2awaitable(awaitUnsafe(if (atMost.isFinite) atMost.toNanos else Long.MaxValue)), atMost)
    }

    @throws(classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
      if (isCompleted || tryAwait(atMost)) this
      else throw new TimeoutException("Futures timed out after [" + atMost.toMillis + "] milliseconds")

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get match {
        case Left(e)  => throw e
        case Right(r) => r
      }

    def value: Option[Either[Throwable, T]] = getState match {
      case c: Either[_, _] => Some(c.asInstanceOf[Either[Throwable, T]])
      case _               => None
    }

    override def isCompleted(): Boolean = getState match { // Cheaper than boxing result into Option due to "def value"
      case _: Either[_, _] => true
      case _               => false
    }

    @inline
    private[this] final def updater = AbstractPromise.updater.asInstanceOf[AtomicReferenceFieldUpdater[AbstractPromise, AnyRef]]

    @inline
    protected final def updateState(oldState: AnyRef, newState: AnyRef): Boolean = updater.compareAndSet(this, oldState, newState)

    @inline
    protected final def getState: AnyRef = updater.get(this)

    def tryComplete(value: Either[Throwable, T]): Boolean = {
      val resolved = resolveEither(value)
      (try {
        @tailrec
        def tryComplete(v: Either[Throwable, T]): List[Future.OnCompleteTask[T]] = {
          getState match {
            case raw: List[_] =>
              val cur = raw.asInstanceOf[List[Future.OnCompleteTask[T]]]
              if (updateState(cur, v)) cur else tryComplete(v)
            case _ => null
          }
        }
        tryComplete(resolved)
      } finally {
        synchronized { notifyAll() } //Notify any evil blockers
      }) match {
        case null             => false
        case cs if cs.isEmpty => true
        case cs               => cs.foreach(c => c.dispatch(resolved)); true
      }
    }

    def onComplete[U](func: Either[Throwable, T] => U)(implicit executor: ExecutionContext): this.type = {
      val bound = new Future.OnCompleteTask[T](executor, func)
      @tailrec //Tries to add the callback, if already completed, it dispatches the callback to be executed
      def dispatchOrAddCallback(): Unit =
        getState match {
          case r: Either[_, _]    => bound.dispatch(r.asInstanceOf[Either[Throwable, T]])
          case listeners: List[_] => if (updateState(listeners, bound :: listeners)) () else dispatchOrAddCallback()
        }
      dispatchOrAddCallback()
      this
    }
  }

  /** An already completed Future is given its result at creation.
   *
   *  Useful in Future-composition when a value to contribute is already available.
   */
  final class KeptPromise[T](suppliedValue: Either[Throwable, T]) extends Promise[T] {

    val value = Some(resolveEither(suppliedValue))

    override def isCompleted(): Boolean = true

    def tryComplete(value: Either[Throwable, T]): Boolean = false

    def onComplete[U](func: Either[Throwable, T] => U)(implicit executor: ExecutionContext): this.type = {
      val completedAs = value.get
      (new Future.OnCompleteTask(executor, func)).dispatch(completedAs)
      this
    }

    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

    def result(atMost: Duration)(implicit permit: CanAwait): T = value.get match {
      case Left(e)  => throw e
      case Right(r) => r
    }
  }

}
