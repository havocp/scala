/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.lang.Thread
import scala.concurrent.util.Duration

/**
 * A context which can be notified by `Future.blocking()` when
 * a thread is about to block. This trait may be implemented
 * by the `Thread.currentThread` instance or provided
 * around a block of code using `BlockContext.push()`.
 */
trait BlockContext {

  /** Used internally by the framework; blocks execution for at most
   * `atMost` time while waiting for an `awaitable` object to become ready.
   *
   *  Clients should use `scala.concurrent.blocking` instead.
   */
  def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T
}

object BlockContext {
  private object DefaultBlockContext extends BlockContext {
    override def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T =
      awaitable.result(atMost)(Await.canAwaitEvidence)
  }

  private val contextLocal = new ThreadLocal[BlockContext]() {
    override def initialValue = Thread.currentThread match {
      case ctx: BlockContext => ctx
      case _ => DefaultBlockContext
    }
  }

  /** Obtain the current thread's current `BlockContext` */
  def current: BlockContext = contextLocal.get

  /** Pushes a current `BlockContext` while executing `body`. */
  def push[T](blockContext: BlockContext)(body: => T): T = {
    val old = contextLocal.get
    try {
      contextLocal.set(blockContext)
      body
    } finally {
      contextLocal.set(old)
    }
  }
}
