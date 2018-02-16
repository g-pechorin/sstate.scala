package peterlavalle.sstate

import java.util

import scala.annotation.tailrec

/** SPEC
	* untested trait to stuff FSM into threads
	*/
trait MachineThread[G, S <: Machine.TState, E <: Machine.TEvent] {

	def !(event: E): Unit

	def ?[O](finish: (G, S) => O): O
}

/** SPEC
	* untested object to stuff FSM into threads
	*/
object MachineThread {
	def apply[G, S <: Machine.TState, E <: Machine.TEvent](shell: Machine.Shell[G, S, E]): MachineThread[G, S, E] = {
		new MachineThread[G, S, E] {

			sealed trait TCommand

			case class SendEvent(e: E) extends TCommand

			case class SendFinish(g: (G, S) => Unit) extends TCommand

			private val queue: util.LinkedList[TCommand] = new util.LinkedList[TCommand]()

			override def !(event: E): Unit = {
				queue.synchronized {
					queue.add(SendEvent(event))
					queue.notify()
				}
			}

			override def ?[O](finish: (G, S) => O): O = {
				var output: Option[O] = None

				object WaitForIt

				WaitForIt.synchronized {
					queue.synchronized {
						queue.add(SendFinish {
							case (g: G, s: S) =>
								WaitForIt.synchronized {
									output = Some(finish(g, s))
									WaitForIt.notify()
								}
						})
						queue.notify()
					}
					WaitForIt.wait()
					output.get
				}
			}

			@tailrec
			private
			final def process(shell: Machine.Shell[G, S, E]): Unit = {

				if (queue.isEmpty)
					queue.wait()

				queue.removeFirst() match {
					case SendEvent(e: E) =>
						process(shell ! e)

					case SendFinish(f) =>
						f(shell.ghost, shell.state)
				}
			}

			RecursiveThread(shell)(process)
		}
	}
}
