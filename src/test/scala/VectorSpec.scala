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

  it should "add itself with another vector" in {
    assert(Vector(1, 2, 3) + Vector(4, 5, 6) == Vector(5, 7, 9))
    assert(Vector(1, 2, 3) + Seq(4, 5, 6) == Vector(5, 7, 9))
  }

  it should "check the dimensions for subtraction" in {
    intercept[IllegalArgumentException] {
      Vector(1, 2, 3) - Vector(4, 5)
    }
  }

  it should "subtract another vector from itself" in {
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

  it should "check the dimensions for performing a dot product" in {
    intercept[IllegalArgumentException] {
      Vector(1, 2, 3) dot Vector(4, 5)
    }
  }

  it should "check the dimensions for calculating the angle between two vectors" in {
    intercept[IllegalArgumentException] {
      Vector(1, 2, 3) angleDegreesWith Vector(4, 5)
    }
  }

  it should "calculate it's dot product with another vector" in {
    assert((Vector(1, 2, 3) dot Vector(4, 5, 6)) == 32)
  }

  it should "calculate the angle between itself and another vector" in {
    assert(((Vector(1, 2, 3) angleRadiansWith Vector(4, 5, 6))*1e8).toInt == 22572612)
    assert(((Vector(1, 2, 3) angleDegreesWith  Vector(4, 5, 6))*1e6).toInt == 12933154)
  }

  it should "throw an exception when trying to calculate an angle with the zero vector" in {
    intercept[ArithmeticException] {
      Vector(0, 0, 0) angleRadiansWith Vector(4, 5, 6)
    }
  }

  it should "not return a NaN due to floating point rounder errors when calculating an angle with another vector" in {
    val coords = Seq[Double](0.70710678118654752440084436210485, 0.70710678118654752440084436210485)
    assert(!(Vector(coords) angleRadiansWith Vector(coords)).isNaN)

    val coords2 = Seq[Double](10.01, 10)
    val coords3 = Seq[Double](-10.01, -10)
    assert(!(Vector(coords2) angleRadiansWith Vector(coords3)).isNaN)
  }

  it should "check if it is parallel with another vector" in {
    val vector = Vector(1, 2, 3)
    assert(vector parallelWith vector * 0.5)
    assert(vector parallelWith vector * -2.5)
    assert(vector parallelWith vector.unitVector)
    assert(vector parallelWith Vector(0, 0, 0))
    assert(!(vector parallelWith Vector(5, 4, -7)))
  }

  it should "check if it is orthogonal with another vector" in {
    val vector = Vector(1, 0)
    assert(vector orthogonalWith Vector(0, 7))
    assert(vector orthogonalWith Vector(0, -1))
    assert(vector orthogonalWith Vector(0, 0))
    assert(!(vector orthogonalWith vector.unitVector))
    val v = Vector(-2.328, -7.284, -1.214)
    val x = Vector(-1.821, 1.072, -2.94)
    assert(v orthogonalWith x)
  }

}
