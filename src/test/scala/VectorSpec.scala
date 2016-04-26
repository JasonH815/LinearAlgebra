import jasonh815.linearalgebra.Vector
import org.scalatest._


class VectorSpec extends FlatSpec with Matchers {

  "A Vector" should "equal a vector with the same values" in {
    assert(Vector(1, 2, 3).equals(Vector(1, 2, 3)))
    assert(Vector(1, 2, 3) == Vector(1, 2, 3))
    assert(!Vector(1, 2, 3).eq(Vector(1, 2, 3)))
  }

  it should "not equal a vector with different values" in {
    assert(Vector(1, 2, 3) != Vector(1, 2, 0))
    assert(Vector(1, 2, 3) != Vector(0, 2, 3))
    assert(Vector(1, 2, 3) != Vector(1, 0, 3))
  }

  it should "print out the coordinates when called with toString" in {
    assert(Vector(Seq(1.0,2.0,3.0)).toString.equals("Vector: (1.0, 2.0, 3.0)"))
  }

  it should "throw an exception when creating an empty vector" in {
    intercept[InstantiationException] {
      Vector(Seq.empty[Double])
    }
  }

  it should "check the dimensions for addition" in {
    intercept[IllegalArgumentException] {
      Vector(1, 2, 3) + Vector(4, 5)
    }
  }

  it should "add two vectors" in {
    assert(Vector(1, 2, 3) + Vector(4, 5, 6) == Vector(5, 7, 9))
    assert(Vector(1, 2, 3) + Seq(4, 5, 6) == Vector(5, 7, 9))
  }

  it should "check the dimensions for subtraction" in {
    intercept[IllegalArgumentException] {
      Vector(1, 2, 3) - Vector(4, 5)
    }
  }

  it should "subtract two vectors" in {
    assert(Vector(1, 2, 3) - Vector(4, 5, 6) == Vector(-3, -3, -3))
    assert(Vector(1, 2, 3) - Seq(4, 5, 6) == Vector(-3, -3, -3))
  }

  it should "perform scalar multiplication" in {
    assert(-2 * Vector(1, 2, 3) == Vector(-2, -4, -6))
    assert(Vector(1, 2, 3) * -2 == Vector(-2, -4, -6))
  }

  it should "return it's magnitude" in {
    assert(Vector(3, 4).magnitude == 5 )
    assert(Vector(0, 0).magnitude == 0)
  }

  it should "return it's unit vector" in {
    // need to account for rounding error.
    // Didn't use scalar multiplication function to avoid having a dependency on another test
    assert(Vector(Vector(3, 4).unitVector.coordinates.map(d => (d*10).toInt)) == Vector(6, 8))
  }

  it should "throw an error when trying to return the unit vector of the zero vector" in {
    intercept[ArithmeticException] {
      Vector(0, 0, 0).unitVector
    }
  }

}
