package dhg.util

object Time {

  def time[T](name: String, block: => T): T = {
    time(name, block, println)
  }

  def time[T, R](name: String, block: => T, log: String => R): T = {
    log(s"starting: $name")
    val (r, t) = timer(block)
    log(s"finished: $name in $t seconds")
    r
  }

  def time1[T](name: String, block: => T): T = {
    time1(name, block, println)
  }

  def time1[T, R](name: String, block: => T, log: String => R): T = {
    val (r, t) = timer(block)
    log(s"$name - $t seconds")
    r
  }

  def timer[T](block: => T): (T, Double) = {
    val startTime = System.currentTimeMillis()
    val r = block
    (r, (System.currentTimeMillis() - startTime) / 1000.0)
  }

}
