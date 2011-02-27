package edu.luc.etl.scala.wavefront

object Main {

  def main(args: Array[String]) = {
    val n = args(0).toInt
    val w = wavefront(n)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        print(w(i)(j))
        print(" ")
      }
      println
    }
  }

  def wavefront(n: Int) = {
    lazy val a: Stream[Stream[Int]] = Stream.tabulate(n, n)((i, j) =>
      if (i == 0)
        j
      else if (j == 0)
        i
      else
        a(i)(j - 1) + a(i - 1)(j - 1) + a(i - 1)(j)
    )
    a
  }
}