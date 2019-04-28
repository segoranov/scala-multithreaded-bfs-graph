package graph

object Timer {
  type ElapsedMilliSeconds = Long

  def time[R](block: => R): (R, ElapsedMilliSeconds)  = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()

    val elapsedTime = t1 - t0
    (result, elapsedTime)
  }
}
