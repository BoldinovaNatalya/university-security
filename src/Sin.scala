import breeze.plot.Figure

object Sin extends App {

  val interval = 15
  val bigProbability = 5.0/6
  val smallProbability = 0.5
  val mathExp = (interval * 60) / 2
  val width = interval * 33.3 // weird constant

  val min = 0.1

  val f = Figure()
  val subplot2 = f.subplot(0)
  subplot2.setXAxisDecimalTickUnits()
  subplot2.setYAxisDecimalTickUnits()
  val X = (0 to 3600).map(_ * 1.0)
  def gauss(x: Double): Double = {
    math.abs(math.sin((x* 2* math.Pi)/1800))
  }




  def bigProb(time: Double) = {

    gauss(time % (interval * 60)) * (bigProbability - min) + min
  }

  def smallProb(time: Double) = {
    gauss(time % (interval * 60)) * (smallProbability - min) + min
  }


  def first(time: Double) = {
    val quarter = (time % (interval * 60 * 4)).toInt / 60 / interval
    if (quarter % 2 == 0) smallProb(time) else bigProb(time)

  }

  def second(time: Double) = {
    val half = (time % 3600).toInt/60/(interval * 2)
    if (half % 2 == 0) smallProb(time) else bigProb(time)
    //    val quarter = (time % 3600).toInt / 60 / 15
    //    if (quarter % 2 == 0) bigProb(time) else smallProb(time)
  }






  println("I'm here")
  val centres = List[Double => Double](second)
    centres.foreach(t =>
    subplot2 += breeze.plot.plot(X, X.map(t), shapes = true)
  )
  //  println("I'm here")
  //  subplot2 += .plot(X, X.map(gauss(0, _)))

  println("I'm here")
  subplot2.xlabel = "Время"
  subplot2.ylabel = "Интенсивность (авт./сек.)"

  f.saveas("intens1.png")
  System.exit(0)

}
