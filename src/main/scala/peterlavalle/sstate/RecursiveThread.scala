package peterlavalle.sstate

/** SPEC
	*/
object RecursiveThread {
	def apply[T](value: T)(code: T => Unit): Unit = {
		new Thread() {
			override def run(): Unit = {
				code(value)
			}
		}.start()
	}
}
