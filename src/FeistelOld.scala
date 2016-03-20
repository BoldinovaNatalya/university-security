
import java.nio.{LongBuffer, ByteBuffer}


object FeistelOld extends App{
  val StepCount = 15
  val iv = 555585842343L

  def circularShiftRight64(k: Long, shift: Int) : Long = {
    val l: Int = 64
    (k >>> shift) | (k << (l - shift))
  }

  def circularShiftLeft64(k: Long, shift: Int) : Long = {
    val l: Int = 64
    (k << shift) | (k >>> (l - shift))
  }

  def circularShiftRight8(k: Byte, shift: Int) : Byte = {
    val l: Int = 8
    ((k >>> shift) | (k << (l - shift))).toByte
  }

  def circularShiftLeft8(k: Byte, shift: Int) : Byte = {
    val l: Int = 8
    ((k << shift) | (k >>> (l - shift))).toByte
  }

  def f(r: Int): Int = {
    val b = ByteBuffer.allocate(4)
    b.putInt(r)
    val bb = ByteBuffer.allocate(4)
    bb.put((~b.get(3)).toByte)
    bb.put((circularShiftRight8(b.get(0), 5) ^ b.get(1)).toByte)
    bb.put((b.get(2) | (~b.get(0))).toByte)
    bb.put((circularShiftLeft8(b.get(3), 3) ^ (~b.get(2))).toByte)
    bb.getInt(0)
  }

  def getOddBits(l: Long): Int = {
    var r = 0L
    for (i <- 0 until 31) {
      val c: Long = 2 << (2 * i)
      r += ((~(l ^ c)) & c) >>> i
    }
    r.toInt
  }


  def getStepKey(key: Long, step: Int): Int = {
    getOddBits(circularShiftRight64(key, step))
  }

  def encryptStep(key: Long, message: Long, step: Int): Long = {
    val b = ByteBuffer.allocate(8)
    b.putLong(message)
    val bb = ByteBuffer.allocate(8)
    bb.putInt(b.getInt(0) ^ getStepKey(key, step))
    bb.putInt(b.getInt(4) ^ f(b.getInt(0) ^ getStepKey(key, step)))
    bb.getLong(0)

  }

  def decryptStep(key: Long, message: Long, step: Int): Long = {
    val b = ByteBuffer.allocate(8)
    b.putLong(message)
    val bb = ByteBuffer.allocate(8)
    bb.putInt(b.getInt(0) ^ getStepKey(key, step))
    bb.putInt(b.getInt(4) ^ f(b.getInt(0)))
    bb.getLong(0)
  }
  def swap(message: Long) : Long = {
    val b = ByteBuffer.allocate(8)
    b.putLong(message)
    val bb = ByteBuffer.allocate(8)
    bb.putInt(b.getInt(4))
    bb.putInt(b.getInt(0))
    bb.getLong(0)
  }

  def encryptLong(key: Long, message: Long) : Long = {
    var currentCipher = message
    for (i <- 0 to StepCount - 2) {
      currentCipher = swap(encryptStep(key, currentCipher, i))
    }
    encryptStep(key, currentCipher, StepCount - 1)
  }

  def decryptLong(key: Long, cipher: Long) : Long = {
    var currentCipher = cipher
    for (i <- 0 to StepCount - 2) {
      currentCipher = swap(decryptStep(key, currentCipher, StepCount - i - 1))
    }
    decryptStep(key, currentCipher, 0)
  }


  def encrypt(key: Long, message: Array[Byte]): Array[Byte] = {
    val capacity = if (message.length % 8 == 0) message.length else message.length + (8 - message.length % 8)
    val buffer = ByteBuffer.allocate(capacity)
    buffer.put(message)
    buffer.clear()
    val arr = new Array[Long](capacity / 8)
    buffer.asLongBuffer().get(arr)
    buffer.clear()
    buffer.asLongBuffer().put(arr.map(encryptLong(key, _)))
    buffer.array()
  }

  def decrypt(key: Long, cipher: Array[Byte]): Array[Byte] = {
    val buffer = ByteBuffer.allocate(cipher.length)
    buffer.put(cipher)
    buffer.clear()
    val arr = new Array[Long](buffer.asLongBuffer().capacity())
    buffer.asLongBuffer().get(arr)
    buffer.clear()
    buffer.asLongBuffer().put(arr.map(decryptLong(key, _)))
    buffer.array()
  }

  val key = 111857465
  val en = encrypt(key, "mother's little helpers".getBytes)
  println(new String(en))
  println(new String(decrypt(key, en)))
  //println(swap(1))
  //println(swap(swap(1)))

}
