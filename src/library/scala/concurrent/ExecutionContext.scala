/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.atomic.{ AtomicInteger }
import java.util.concurrent.{ Executors, Future => JFuture, Callable, ExecutorService, Executor }
import scala.concurrent.util.Duration
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }
import scala.collection.generic.CanBuildFrom
import collection._



trait ExecutionContext {
  
  /** Runs a block of code on this execution context.
   */
  def execute(runnable: Runnable): Unit
  
  /** Reports that an asynchronous computation failed.
   */
  def reportFailure(t: Throwable): Unit
  
}

/**
 * Union interface since Java does not support union types
 */
trait ExecutionContextExecutor extends ExecutionContext with Executor

/**
 * Union interface since Java does not support union types
 */
trait ExecutionContextExecutorService extends ExecutionContextExecutor with ExecutorService


/** Contains factory methods for creating execution contexts.
 */
object ExecutionContext {

  implicit def defaultExecutionContext: ExecutionContext = scala.concurrent.defaultExecutionContext
    
  /** Creates an `ExecutionContext` from the given `ExecutorService`.
   */
  def fromExecutorService(e: ExecutorService, reporter: Throwable => Unit): ExecutionContextExecutorService =
    impl.ExecutionContextImpl.fromExecutorService(e, reporter)

  /** Creates an `ExecutionContext` from the given `ExecutorService` with the default Reporter.
   */
  def fromExecutorService(e: ExecutorService): ExecutionContextExecutorService = fromExecutorService(e, defaultReporter)
  
  /** Creates an `ExecutionContext` from the given `Executor`.
   */
  def fromExecutor(e: Executor, reporter: Throwable => Unit = defaultReporter): ExecutionContextExecutor =
    impl.ExecutionContextImpl.fromExecutor(e, reporter)

 /** Creates an `ExecutionContext` from the given `Executor` with the default Reporter.
   */
  def fromExecutor(e: Executor): ExecutionContextExecutor = fromExecutor(e, defaultReporter)
  
  def defaultReporter: Throwable => Unit = { case t => t.printStackTrace() }
}


