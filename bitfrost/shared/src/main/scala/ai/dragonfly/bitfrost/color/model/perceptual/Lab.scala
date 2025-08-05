package ai.dragonfly.bitfrost.color.model.perceptual

import narr.*
import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.cie.Constant.*
import slash.vector.*
import slash.{Random, cubeInPlace}

trait Lab { self: WorkingSpace =>

  // use opaque types just like Vec[3] ?
  object Lab extends PerceptualSpace[Lab] {

    opaque type Lab = Vec[3]

    def apply(values: NArray[Double]): Lab = dimensionCheck(values, 3).asInstanceOf[Lab]

    /**
     * @param L the L* component of the CIE L*a*b* color.
     * @param a the a* component of the CIE L*a*b* color.
     * @param b the b* component of the CIE L*a*b* color.
     * @return an instance of the LAB case class.
     * @example {{{ val c = LAB(72.872, -0.531, 71.770) }}}
     */
    def apply(L: Double, a: Double, b: Double): Lab = apply(NArray[Double](a, b, L))

    inline def f(t: Double): Double = if (t > ϵ) Math.cbrt(t) else (t * `k/116`) + `16/116`

    /**
     * Requires a reference 'white' because although black provides a lower bound for XYZ values, they have no upper bound.
     *
     * @param xyz
     * @param illuminant
     * @return
     */
    def fromXYZ(xyz: XYZ): Lab = {
      val fy: Double = f(illuminant.`1/yₙ` * xyz.y)

      apply(
        116.0 * fy - 16.0,
        500.0 * (f(illuminant.`1/xₙ` * xyz.x) - fy),
        200.0 * (fy - f(illuminant.`1/zₙ` * xyz.z))
      )
    }

//    override val rgbGamut:Gamut = Gamut.fromRGB(transform = (v:XYZ) => Vector3(fromXYZ(v).values))
//    override def toString:String = s"${illuminant}L*a*b*"

    def L(lab: Lab): Double = lab(2)

    def a(lab: Lab): Double = lab(0)

    def b(lab: Lab): Double = lab(1)

    override def fromVec(v: Vec[3]): Lab = v

    override def toVec(c: Lab): Vec[3] = c.asInstanceOf[Vec[3]].copy
  }

//  case class Lab private(override val values: NArray[Double]) extends PerceptualColorModel[Lab] {
//    override type VEC = this.type with Lab

  type Lab = Lab.Lab

  given PerceptualColorModel[Lab] with {
    extension (lab: Lab) {

      override inline def copy: Lab = Lab(a, b, L)

      def L: Double = Lab.L(lab)

      def a: Double = Lab.a(lab)

      def b: Double = Lab.b(lab)

      def fInverse(t: Double): Double = if (t > `∛ϵ`) cubeInPlace(t) else (`116/k` * t) - `16/k`

      def toXYZ: XYZ = {
        //val white: XYZ = whitePoint //XYZ(illuminant.whitePointValues)
        val fy: Double = `1/116` * (L + 16.0)

        XYZ(
          fInverse((0.002 * a) + fy) * illuminant.xₙ, // X
          (if (L > kϵ) {
            val l = L + 16.0;
            `1/116³` * (l * l * l)
          } else `1/k` * L) * illuminant.yₙ, // Y
          fInverse(fy - (0.005 * b)) * illuminant.zₙ, // Z
        )
      }

      override def similarity(that: Lab): Double = {
        Lab.similarity(lab, that)
      }

      override def toRGB: RGB = toXYZ.toRGB

      override def render: String = s"L*a*b*($L,$a,$b)"
    }
  }
}
