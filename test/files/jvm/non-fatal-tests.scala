import scala.util.control.NonFatal

// tests the basic combinators on Try
trait NonFatalTests {

	//NonFatals
    val SO: Throwable = new StackOverflowError
    val RE: Throwable = new RuntimeException
    val E: Throwable = new Exception
    val T: Throwable = new Throwable
    
    //Fatals
    val Interrupted: Throwable = new InterruptedException
    val OOME: Throwable = new OutOfMemoryError
    val LinkError: Throwable = new LinkageError
    val VME: Throwable = new VirtualMachineError {}
    val Control: Throwable = new Throwable with scala.util.control.ControlThrowable
    val NIE: Throwable = new NotImplementedError

	def testFatalsUsingApply(): Unit = {
       assert(!NonFatal(OOME))
       assert(!NonFatal(Interrupted))
       assert(!NonFatal(LinkError))
       assert(!NonFatal(VME))
       assert(!NonFatal(Control))
       assert(!NonFatal(NIE))
	}

	def testNonFatalsUsingApply(): Unit = {
       assert(NonFatal(SO))
       assert(NonFatal(RE))
       assert(NonFatal(E))
       assert(NonFatal(T))
	}

	def testFatalsUsingUnapply(): Unit = {
       assert(NonFatal.unapply(OOME).isEmpty)
       assert(NonFatal.unapply(Interrupted).isEmpty)
       assert(NonFatal.unapply(LinkError).isEmpty)
       assert(NonFatal.unapply(VME).isEmpty)
       assert(NonFatal.unapply(Control).isEmpty)
       assert(NonFatal.unapply(NIE).isEmpty)
	}

	def testNonFatalsUsingUnapply(): Unit = {
       assert(NonFatal.unapply(SO).isDefined)
       assert(NonFatal.unapply(RE).isDefined)
       assert(NonFatal.unapply(E).isDefined)
       assert(NonFatal.unapply(T).isDefined)
	}

	testFatalsUsingApply()
	testNonFatalsUsingApply()
	testFatalsUsingUnapply()
	testNonFatalsUsingUnapply()
}

object Test
extends App
with NonFatalTests {
  System.exit(0)
}