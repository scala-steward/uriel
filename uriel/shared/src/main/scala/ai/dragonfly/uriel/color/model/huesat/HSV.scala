package ai.dragonfly.uriel.color.model.huesat

import narr.*
import ai.dragonfly.uriel.*
import ai.dragonfly.uriel.cie.WorkingSpace
import ai.dragonfly.uriel.color.model.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import slash.Random
import slash.vector.*

trait HSV extends HueSaturation { self: WorkingSpace =>

  object HSV extends HueSaturationSpace[HSV] {

    opaque type HSV = Vec[3]

    override lazy val usableGamut: Gamut = new Gamut(Cylinder(capSegments = 6))

    def apply(values: NArray[Double]): HSV = dimensionCheck(values, 3).asInstanceOf[HSV]

    def clamp(values: NArray[Double]): HSV = {
      dimensionCheck(values.length, 3)
      clamp(values(0), values(1), values(2))
    }

    /**
     * HSV is the primary case class for representing colors in HSV space.
     *
     * @constructor Create a new HSV object from three Double values.  This constructor does not validate
     *              input parameters.  For values taken from user input, sensors, or otherwise uncertain sources, consider using
     *              the factory method in the Color companion object.
     * @see [[ai.dragonfly.color.HSV.getIfValid]] for a method of constructing HSV objects that validates inputs.
     * @see [[https://en.wikipedia.org/wiki/HSL_and_HSV]] for more information about the HSV color space.
     * @param hue        an angle ranging from [0-360] degrees.  Values outside of this range may cause errors.
     * @param saturation a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @param value      a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @return an instance of the HSV case class.
     * @example {{{
     * val c = HSV(211f, 75f, 33.3333f)
     * c.toString()  // returns "HSV(211.000,75.000,33.333)"
     * }}}
     */

    def apply(hue: Double, saturation: Double, value: Double): HSV = {
      NArray[Double](hue, saturation, value).asInstanceOf[HSV]
    }

    def clamp(hue: Double, saturation: Double, value: Double): HSV = NArray[Double](
      clampHue(hue),
      clamp0to1(saturation),
      clamp0to1(value)
    ).asInstanceOf[HSV]

    /**
     * Factory method for creating instances of the HSV class.  This method validates input parameters and throws an exception
     * if one or more of them lie outside of their allowed ranges.
     *
     * @param saturation an angle ranging from [0-360] degrees.
     * @param hue        a percentage ranging from [0-100].
     * @param value      a percentage ranging from [0-100].
     * @return an instance of the HSV case class.
     */
    def getIfValid(hue: Double, saturation: Double, value: Double): Option[HSV] = {
      if (validHue(hue) && valid0to1(saturation) && valid0to1(saturation)) Some(apply(hue, saturation, value))
      else None
    }

    def fromRGB(nrgb: RGB): HSV = toHSV(nrgb.red, nrgb.green, nrgb.blue)

    inline def toHSV(red: Double, green: Double, blue: Double): HSV = {
      val values: NArray[Double] = hueMinMax(red, green, blue)
      values(1) = {  // S
        if (values(2 /*MAX*/) == 0.0) 0.0
        else (values(2 /*MAX*/) - values(1 /*min*/)) / values(2 /*MAX*/)
      }
      values.asInstanceOf[HSV]
    }

    override def random(r: scala.util.Random = Random.defaultRandom): HSV = apply(
      NArray[Double](
        r.nextDouble() * 360.0,
        r.nextDouble(),
        r.nextDouble()
      )
    )
    override def toRGB(c: HSV): RGB = c.toRGB

    override def toXYZ(c: HSV): XYZ = c.toXYZ

    override def toVec(hsv: HSV): Vec[3] = Vec[3](
      hsv(1) * Math.cos(slash.degreesToRadians(hsv(0))),
      hsv(1) * Math.sin(slash.degreesToRadians(hsv(0))),
      hsv(2)
    )

    def hue(hsv: HSV): Double = hsv(0)

    def saturation(hsv: HSV): Double = hsv(1)

    def value(hsv: HSV): Double = hsv(2)

  }

  type HSV = HSV.HSV

  given CylindricalColorModel[HSV] with {
    extension (hsv: HSV) {

      //case class HSV private(override val values: NArray[Double]) extends HueSaturation[HSV] {

      def hue: Double = HSV.hue(hsv)

      def saturation: Double = HSV.saturation(hsv)

      def value: Double = HSV.value(hsv)

      // https://www.rapidtables.com/convert/color/hsv-to-rgb.html
      def toRGB: RGB = {
        val C = value * saturation
        HSV.hcxmToRGBvalues(hue, C, HSV.XfromHueC(hue, C), value - C).asInstanceOf[RGB]
      }

      override def copy: HSV = NArray[Double](hue, saturation, value).asInstanceOf[HSV]

      override def similarity(that: HSV): Double = HSV.similarity(hsv, that)

      override def render: String = s"HSV($hue, $saturation, $value)"

      override def toXYZ: XYZ = toRGB.toXYZ
    }

  }
}
