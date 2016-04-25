package com.jason.linearalgebra

class Vector(val coordinates:Seq[Double]) {

  if (coordinates.isEmpty) throw new InstantiationException("The coordinates must be nonempty")

  val dimension = this.coordinates.length

  override def toString: String = "Vector: " + coordinates.mkString("(", ", ", ")")

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[Vector]) false else obj.asInstanceOf[Vector].coordinates.equals(this.coordinates)
  }

  private def zipCoords(other:Vector):Seq[(Double, Double)] = {
    if (this.dimension != other.dimension) throw new IllegalArgumentException("Vectors must have the dame dimension")
    this.coordinates.zip(other.coordinates)
  }

  def plus(other:Vector):Vector = Vector(this.zipCoords(other).map(pair => pair._1 + pair._2))

  def +(other:Vector):Vector = this.plus(other)

  def minus(other:Vector):Vector = Vector(this.zipCoords(other).map(pair => pair._1 - pair._2))

  def -(other:Vector):Vector = this.minus(other)

  def scalarMultiply(scalar:Double):Vector = Vector(this.coordinates.map( a => a * scalar))
}


object Vector {

  case class DoubleSequence(seq:Seq[Double])
  case class IntSequence(seq:Seq[Int])

  implicit def ds(seq:Seq[Double]):DoubleSequence = DoubleSequence(seq)
  implicit def is(seq:Seq[Int]):IntSequence = IntSequence(seq)
  implicit def dv(seq:Seq[Double]):Vector = Vector(seq)
  implicit def iv(seq:Seq[Int]):Vector = Vector(seq)


  def apply(x: Double*) = new Vector(x)
  def apply(x: DoubleSequence) = new Vector(x.seq)
  def apply(x: IntSequence) = new Vector(x.seq.map(_.toDouble))
}
