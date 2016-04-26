package jasonh815.linearalgebra

/**
  * Extends the base double class to perform operations on vetors
  */
class ScalarDouble(val d: Double) {
  def *(vector: Vector):Vector = vector * d
}
