import java.io.{FileOutputStream, FileInputStream}

import breeze.plot.Figure
import com.sksamuel.scrimage.{PixelTools, Image}

import scala.io.Source

object Kutter extends App {
  val message = 1234567899999974123L
  val factors = Seq[Double](0.3, 0.59, 0.11)
  val lamda = 0.02
  val _sigma = 1

  val lamdas = for (i <- 1 to 10) yield i * 0.01
  val sigmas = for (i <- 2 to 10) yield i

  val in = new FileInputStream("pony.jpg")
  val out = new FileOutputStream("newpony.png")
  var image = Image(in).toMutable

  def convert(num: Long) = {
    //num.toBinaryString.map(_ - 48)
    for (i <- 0 until 64) yield ((num >>> i) & 1).toInt
  }

  def changeBlue(x: Int, y: Int, sign: Int, lamda: Double) = {
    image.setPixel(x, y,
      PixelTools.rgb(image.rgb(x, y)(0), image.rgb(x, y)(1),
        (image.rgb(x, y)(2) + sign * lamda * factors.zip(image.rgb(x, y)).map(x => x._1 * x._2).sum).toInt))

  }

  def getBarrier(x: Int, y: Int, sigma: Int) = {
    var sum = 0
    for (i <- 0 until sigma) {
      //println(x, y)
      sum += image.rgb(x, y + i)(2) + image.rgb(x, y - i)(2) + image.rgb(x + i, y)(2) + image.rgb(x - i, y)(2)
    }
    sum / (4 * sigma)
  }

  def embed(message: Long, lamda: Double, sigma: Int) = {
    image = Image(new FileInputStream("pony.jpg")).toMutable
    val binaryMessage = convert(message)
    for (i <- 0 until binaryMessage.length) {
      if (binaryMessage(i) == 1) changeBlue(2*i + sigma, 2*i + sigma, 1, lamda) else changeBlue(2*i + sigma, 2*i + sigma, -1, lamda)
    }
    image.write(new FileOutputStream("newpony.jpg"))
  }

  def extract(message: Long, sigma: Int) = {
    for (i <- 0 until convert(message).length) yield {
      if (image.rgb(2*i + sigma, 2*i + sigma)(2) > getBarrier(2*i + sigma, 2*i + sigma, sigma)) 1 else 0
    }
  }

  def mse(m1: Seq[Int], m2: Seq[Int]) = {
    m1.zip(m2).map(x => math.pow(x._1 - x._2, 2)).sum / m1.length
  }

  val lamdaerrors = lamdas.map(x => {
    embed(message, x, _sigma)
    mse(convert(message), extract(message, _sigma))
  })

  val sigmaerrors = sigmas.map(x => {
    embed(message, lamda, x)
    mse(convert(message), extract(message, x))
  })

  val subplot1 = Figure().subplot(0)

  subplot1.setXAxisDecimalTickUnits()
  subplot1.setYAxisDecimalTickUnits()


   subplot1 += breeze.plot.plot(lamdas, lamdaerrors)


  val subplot2 = Figure().subplot(0)
  subplot2.setXAxisIntegerTickUnits()
  subplot2.setYAxisDecimalTickUnits()

  subplot2 += breeze.plot.plot(sigmas.map(_.toDouble), sigmaerrors)

//  println(convert(message))
//  embed(message, lamda, sigma)
//  println(extract(message))
//  println(mse(convert(message), extract(message)))

}

