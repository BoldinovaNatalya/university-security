import scala.collection.mutable.ArrayBuffer

object Rsa extends App {
  val n = BigInt(565570077646207L)

  val e = BigInt(12341)
  val y = "277140870674302260217431481485329310844916399448964498705119"
  val p = BigInt(1546379)
  val q = BigInt(365738333)
  val d = e.modInverse((p-1)*(q-1))
  println(d)
  var j = 0
  val cypher = for {
    i <- 1 until y.length - 1
    if BigInt(y.substring(j, i)) < n && BigInt(y.substring(j, i+1)) > n
  } yield { val result = BigInt(y.substring(j, i)); j = i; result }

  val message = cypher.map(_.modPow(d, n))
  println(message)
  val symbols = ArrayBuffer[Char]()

  message.reverse.foreach(x => {
    var current = x
    while (current != BigInt(0)) {
      val symbol = current % 100
      symbols += symbol.toChar
      current = current / 100
    }
  })

  println(new String(symbols.reverse.toArray))
}
