package graph

object Timer {
  type TimeElapsedInMilliseconds = Long

  def time[R](block: => R): (R, TimeElapsedInMilliseconds)  = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()

    val elapsedTime = t1 - t0
    (result, elapsedTime)
  }
}
