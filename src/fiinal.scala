import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageIO

import breeze.plot.Figure
import com.sksamuel.scrimage.{PixelTools, Image}

import scala.collection.mutable.ListBuffer

import javax.imageio.plugins.jpeg.JPEGHuffmanTable

object fiinal extends App {


  def lsbEnhancement(color: Color)  = {


    if ((color.getBlue & 0x01) == 1) Color.black else Color.white

  }

  def lsbEnhancementAttack(image: BufferedImage) = {
    val resultImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_ARGB)

    for (i <-0 until image.getWidth) {
      for (j <-0 until image.getHeight) {
        resultImage.setRGB(i, j, lsbEnhancement(new Color(image.getRGB(i, j))).getRGB)
      }
    }
    resultImage
  }


  def getBrightness(x: Int) = {
    val color = new Color(x)
    val red = color.getRed
    val green = color.getGreen
    val blue = color.getBlue
    red * 0.3 + green * 0.6 + blue * 0.1
  }

  def divideToBlocks(image: BufferedImage) = {
    var blocks = ListBuffer[Array[Array[Double]]]()
    for (i <- 0 until image.getWidth by 8) {
      for (j <- 0 until image.getHeight by 8) {
        val si =  image.getSubimage(i, j, 8, 8)
        var block = new Array[Array[Double]](8)
        for (k <- 0 until 8) {
         block(k) =  (for (l <- 0 until 8 )  yield  getBrightness(si.getRGB(k, l))).toArray
        }
        blocks += block
      }
    }

    blocks

  }

  def divideToLines(image: BufferedImage) = {
   val lines =  (for ( i<- 0 until image.getWidth) yield
      for ( j <- 0 until image.getHeight)  yield image.getRGB(i, j)).flatten
    for (i <- 0 until lines.length / 8) yield lines.slice(i*8, 8*(i+1))
  }


  def F0(x : Int) = {
    x
  }

  def F1(x: Int) = {
    if (x % 2 == 0) x + 1 else x - 1
  }

  def F2(x : Int) = {
    if (x % 2 != 0) x + 1 else x - 1
  }

  def G(p: Seq[Int]) = {
    (for (i <- 1 until p.length) yield math.abs(p(i - 1) - p(i))).sum
  }

  def rs (image: BufferedImage) = {
    val groups = divideToLines(image)
    var sing1 = 0
    var sing2 =0
    var reg1 =0
    var reg2 =0


    for (g <- groups) {
     val g0 = G(g)
      val g1 = G(g.map(F1))
      val g2 = G(g.map(F2))
      if (g1 > g0 ) sing1 += 1
      if (g2 > g0) sing2 += 1
      if(g1 < g0) reg1 += 1
      if(g2 < g0) reg2 += 1

    }

    (sing1, sing2, reg1, reg2)
  }


  for (i <- 1 until 9) {
    val in = new File(s"./img/000$i.jpg")
    val image = ImageIO.read(in)
    val res = lsbEnhancementAttack(image)
    println(s"${in.getName} : ${rs(image)}")



    ImageIO.write(res, "jpg", new File(s"./img/result$i.jpg"))
  }


  val in = new File(s"./img/0001.jpg")
  val image = ImageIO.read(in)
  val dct = new DCT()
  val dcts = divideToBlocks(image).map(dct.applyDCT).flatten.flatten.filter(math.abs(_) < 10)
  val f = Figure()
  val subplot = f.subplot(0)
  subplot += breeze.plot.hist(dcts, 1000)
  f.saveas(s"FIGURE1.png")














}
