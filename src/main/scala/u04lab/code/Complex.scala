package u04lab.code

trait Complex:
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers

case class ComplexImpl(re: Double, im: Double) extends Complex:
  override def +(c: Complex): Complex  = ComplexImpl(this.re + c.re, this.im + c.im)
  override def *(c: Complex): Complex = ComplexImpl(this.re * c.re - this.im * c.im, this.re * c.im + this.im * c.re)

object Complex:
  def apply(re: Double, im: Double): Complex = ComplexImpl(re, im)

@main def checkComplex(): Unit =
  val a = Array(Complex(10, 20), Complex(1, 1), Complex(7, 0))
  val c = a(0) + a(1) + a(2)
  println((c, c == ComplexImpl(18.0,21.0), c.re, c.im)) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println((c2.toString,  c2 == ComplexImpl(-10.0,30.0),  c2.re, c2.im)) // (ComplexImpl(-10.0,30.0),-10.0,30.0)

/** Hints:
  *   - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  *   - check that equality and toString do not work
  *   - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  *   - check equality and toString now
  */
