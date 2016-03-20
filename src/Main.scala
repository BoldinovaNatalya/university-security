import breeze.plot._

import math._
import scala.collection._

class RandGenerator extends Iterator[Double] {
  private val M = pow(5, 15)
  private val p = 174157.0
  private val u = 173807.0
  private var currentR = u / p

  override def hasNext: Boolean = true

  override def next(): Double = {
    currentR = (M * currentR) % 1
    currentR
  }
}

object Main extends App {
  val n = 100000
  val beans = 10
  val generator = new RandGenerator
  val sequence = generator.take(n).toSeq
  val mathExp = sequence.sum / sequence.length
  println(mathExp)
  val D = sequence.map(x => pow(mathExp - x, 2)).sum/sequence.length
  println(D)
  val f = Figure()
  val p = f.subplot(0)
  p += breeze.plot.hist(sequence, beans)

 val frequencies = breeze.stats.hist(sequence, beans).hist
  println(frequencies.map(x => pow(x - n/beans,2)/(n/beans)).sum)

  val seq = sequence.map(x => floor(x*10))
//val seq = sequence.map(x => java.lang.Double.doubleToRawLongBits(x).toBinaryString.toCharArray.slice(0, 31)).flatten

  var dictionary = mutable.Map[Int, Int]()

  var counter = 1
  var i = 0
  while(i < seq.length){
    while((i < seq.length - 1) && (seq(i) == seq(i+1))) {
      counter += 1
      i += 1
    }
    dictionary(counter) = dictionary.getOrElse(counter, 0) + 1
    counter = 1
    i += 1
  }

  val s = dictionary.toSeq.sortBy(_._1)
  println(s)
  val x = s.map(_._1)
  val y = s.map(_._2)

  Figure().subplot(0) += breeze.plot.plot(x, y)
}
