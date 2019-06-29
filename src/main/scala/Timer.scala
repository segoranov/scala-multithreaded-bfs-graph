package graph

case class ResultWithTimeElapsed[R](result: R, timeElapsedInMilliseconds: Long)

object Timer {
  def time[R](block: => R): ResultWithTimeElapsed[R] = {
    val t0 = System.currentTimeMillis
    val result = block // call-by-name
    val t1 = System.currentTimeMillis

    val elapsedTime = t1 - t0
    ResultWithTimeElapsed(result, elapsedTime)
  }
}
