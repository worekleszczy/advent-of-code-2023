package worekleszczy.aoc.day6

object Puzzle extends App {

  def calculate(t: Double, p: Double): (Long, Long) = {
    val delta = math.pow(t, 2) - (4 * p)

    val x1 = (-t - math.sqrt(delta)) / -2.0
    val x2 = (-t + math.sqrt(delta)) / -2.0

    (math.ceil(x2 + 0.00001f).toLong, math.floor(x1 - 0.00001f).toLong)
  }

  def combination(time: Double, record: Double): Long = {
    val (min, max) = calculate(time, record)

    (min to max).size
  }
  /*
      (2.0,5.0)
      (4.0,11.0)
      (11.0,19.0)
   */
  val r1 = combination(48, 390)
  val r2 = combination(98, 1103)
  val r3 = combination(90, 1112)
  val r4 = combination(83, 1360)
  println(r1 * r2 * r3 * r4)
  println(combination(71530,940200))
  println(combination(48989083,390110311121360L))

}
