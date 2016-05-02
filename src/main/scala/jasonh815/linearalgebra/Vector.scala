package jasonh815.linearalgebra

import scala.language.implicitConversions

/**
  * Implementation of the mathematical concept of a vector
  */
class Vector(val coordinates:Seq[Double]) {

  if (coordinates.isEmpty) throw new InstantiationException("The coordinates must be nonempty")

  val dimension:Int = this.coordinates.length

  override def toString: String = "Vector: " + coordinates.mkString("(", ", ", ")")

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[Vector]) false else obj.asInstanceOf[Vector].coordinates.equals(this.coordinates)
  }

  /** zips the elements of two vectors together */
  private def zipCoords(other:Vector):Seq[(Double, Double)] = {
    if (this.dimension != other.dimension) throw new IllegalArgumentException("Vectors must have the dame dimension")
    this.coordinates.zip(other.coordinates)
  }

  /** add elements of two vectors */
  def plus(other:Vector):Vector = Vector(this.zipCoords(other).map(pair => pair._1 + pair._2))

  /** add elements of two vectors */
  def +(other:Vector):Vector = this.plus(other)

  /** subtract elements of two vectors */
  def minus(other:Vector):Vector = Vector(this.zipCoords(other).map(pair => pair._1 - pair._2))

  /** subtract elements of two vectors */
  def -(other:Vector):Vector = this.minus(other)

  /** multiply elements of a vector by a scalar */
  def scalarMultiply(scalar:Double):Vector = Vector(this.coordinates.map( a => a * scalar))

  /** multiply elements of a vector by a scalar */
  def *(scalar:Double):Vector = this.scalarMultiply(scalar)

  /** returns the magnitude of the vector */
  lazy val magnitude:Double = math.sqrt(this.coordinates.map(c => math.pow(c, 2)).sum)

  /** returns a unit vector of the current vector which points in the same direction with magnitude 1 */
  lazy val unitVector:Vector = {
    if(magnitude == 0) throw new ArithmeticException("The zero vector has no unit vector")

    1/this.magnitude * this
  }
  /** returns the dot product of two vectors */
  def dot(other:Vector):Double = this.zipCoords(other).map(pair => pair._1 * pair._2).sum

  /** returns the angle in radians between two vectors */
  def angleRadiansWith(other: Vector):Double = math.acos(dot(other)/(this.magnitude * other.magnitude))

  /** returns the angle in degrees between two vectors */
  def angleDegreesWith(other: Vector):Double = angleRadiansWith(other).toDegrees

}

object Vector {

  // wrapper for int and double sequences to handle type erasure and allow for overloading apply methods below
  case class DoubleSequence(seq:Seq[Double])
  case class IntSequence(seq:Seq[Int])
  implicit def ds(seq:Seq[Double]):DoubleSequence = DoubleSequence(seq)
  implicit def is(seq:Seq[Int]):IntSequence = IntSequence(seq)

  // implicitly use sequences as vectors
  implicit def dv(seq:Seq[Double]):Vector = Vector(seq)
  implicit def iv(seq:Seq[Int]):Vector = Vector(seq)

  // extend int and double primitives
  implicit def int2ScalarDouble(int: Int):ScalarDouble = new ScalarDouble(int.toDouble)
  implicit def double2ScalarDouble(double:Double):ScalarDouble = new ScalarDouble(double)

  // vector factory methods
  def apply(x: Double*) = new Vector(x)
  def apply(x: DoubleSequence) = new Vector(x.seq)
  def apply(x: IntSequence) = new Vector(x.seq.map(_.toDouble))
}
