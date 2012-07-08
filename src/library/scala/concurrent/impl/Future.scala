/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import scala.concurrent.util.Duration
import scala.concurrent.{Awaitable, ExecutionContext, CanAwait}
import scala.collection.mutable.Stack
import scala.util.control.NonFatal


private[concurrent] trait Future[+T] extends scala.concurrent.Future[T] with Awaitable[T] {

}

private[concurrent] object Future {
  import java.{ lang => jl }

  private val toBoxed = Map[Class[_], Class[_]](
    classOf[Boolean] -> classOf[jl.Boolean],
    classOf[Byte]    -> classOf[jl.Byte],
    classOf[Char]    -> classOf[jl.Character],
    classOf[Short]   -> classOf[jl.Short],
    classOf[Int]     -> classOf[jl.Integer],
    classOf[Long]    -> classOf[jl.Long],
    classOf[Float]   -> classOf[jl.Float],
    classOf[Double]  -> classOf[jl.Double],
    classOf[Unit]    -> classOf[scala.runtime.BoxedUnit]
  )

  /** Wraps a block of code into an awaitable object. */
  private[concurrent] def body2awaitable[T](body: =>T) = new Awaitable[T] {
    def ready(atMost: Duration)(implicit permit: CanAwait) = {
      body
      this
    }
    def result(atMost: Duration)(implicit permit: CanAwait) = body
  }
  
  def boxedType(c: Class[_]): Class[_] = if (c.isPrimitive) toBoxed(c) else c

  // TODO rename appropriately and make public
  private[concurrent] def isFutureThrowable(t: Throwable) = t match {
    case e: Error                               => false
    case t: scala.util.control.ControlThrowable => false
    case i: InterruptedException                => false
    case _                                      => true
  }

  private[impl] class PromiseCompletingTask[T](override val executor: ExecutionContext, body: => T)
    extends Task {
    val promise = new Promise.DefaultPromise[T]()

    protected override def task() = {
      promise complete {
        try Right(body) catch {
          case NonFatal(e) =>
            // Commenting out reporting for now, since it produces too much output in the tests
            //executor.reportFailure(e)
            Left(e)
        }
      }
    }
  }

  def apply[T](body: =>T)(implicit executor: ExecutionContext): Future[T] = {
    val task = new PromiseCompletingTask(executor, body)
    task.dispatch()
    task.promise.future
  }

  // an optimization for batching futures
  // TODO we should replace this with a public queue,
  // so that it can be stolen from
  // OR: a push to the local task queue should be so cheap that this is
  // not even needed, but stealing is still possible

  private[impl] case class TaskStack(stack: Stack[Task], executor: ExecutionContext)

  private val _taskStack = new ThreadLocal[TaskStack]()

  private[impl] trait Task extends Runnable {
    def executor: ExecutionContext

    // run the original callback (no dispatch)
    protected def task(): Unit

    // we implement Runnable to avoid creating
    // an extra object. run() runs ourselves with
    // a TaskStack pushed, and then runs any
    // other tasks that show up in the stack.
    final override def run() = {
      try {
        val taskStack = TaskStack(Stack[Task](this), executor)
        _taskStack set taskStack
        while (taskStack.stack.nonEmpty) {
          val next = taskStack.stack.pop()
          try {
            require(next.executor eq executor)
            next.task()
          } catch { case NonFatal(e) => executor reportFailure e }
        }
      } finally {
        _taskStack.remove()
      }
    }

    // send the task to the running executor.execute() via
    // _taskStack, or start a new executor.execute()
    def dispatch(force: Boolean = false): Unit =
      _taskStack.get match {
        case stack if (stack ne null) && (executor eq stack.executor) && !force => stack.stack push this
        case _ => executor.execute(this)
      }
  }

  private[impl] class ReleaseTask(override val executor: ExecutionContext, val elems: List[Task]) extends Task {
    protected override def task(): Unit = _taskStack.get match {
      case null => executor.reportFailure(new NullPointerException("taskStack is null"))
      case taskStack => taskStack.stack.elems ++= elems
    }
  }

  private[impl] def releaseStack(): Unit =
    _taskStack.get match {
      case null => // do nothing - there is no local batching stack anymore
      case stack =>
        _taskStack.remove()
        val tasks = stack.stack.elems
        if (tasks.nonEmpty) {
          stack.stack.clear()
          (new ReleaseTask(stack.executor, tasks)).dispatch(force = true)
        }
    }

  private[impl] class OnCompleteTask[T](override val executor: ExecutionContext, val onComplete: (Either[Throwable, T]) => Any)
    extends Task {
    private[this] var value: Either[Throwable, T] = null

    protected override def task() = {
      require(value ne null) // dispatch(value) must be called before dispatch()
      onComplete(value)
    }

    def dispatch(value: Either[Throwable, T]): Unit = {
      this.value = value
      dispatch()
    }
  }
}
