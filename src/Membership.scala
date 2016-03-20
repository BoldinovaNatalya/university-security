import breeze.plot.Figure

object  Membership extends App {
  val f = Figure()
  val subplot2 = f.subplot(0)
  subplot2.setXAxisDecimalTickUnits()
  subplot2.setYAxisIntegerTickUnits()
  val X = (0 to 100).map(_*0.01)
def gauss(mathExp: Double, x: Double): Double = {
  math.exp(-(math.pow(x - mathExp, 2)/math.pow(2.0 / 15, 2)))
}
  println("I'm here")
  val names = List[String]("Мало", "Средне", "Много", "Очень много")
  val centres = List[Double](0.0, 1.0/3, 2.0/3, 1.0)
  centres.zip(names).foreach(t =>
    subplot2 += breeze.plot.plot(X, X.map(gauss(t._1, _)), shapes = true, name = t._2)
  )
//  println("I'm here")
//  subplot2 += .plot(X, X.map(gauss(0, _)))

  println("I'm here")
  subplot2.xlabel = "t"
  subplot2.ylabel = "μ(t)"

  f.saveas("membershipdots4.png")
  System.exit(0)
  //subplot2 += breeze.plot.plot(x, sigmaerrors)
}
