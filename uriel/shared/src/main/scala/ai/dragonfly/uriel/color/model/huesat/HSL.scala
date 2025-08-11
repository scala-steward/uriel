package ai.dragonfly.uriel.color.model.huesat

import narr.*
import ai.dragonfly.uriel.*
import ai.dragonfly.uriel.cie.WorkingSpace
import ai.dragonfly.uriel.color.model.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import slash.Random
import slash.vector.*

trait HSL extends HueSaturation { self: WorkingSpace =>
  object HSL extends HueSaturationSpace[HSL] {

    opaque type HSL = Vec[3]

    def apply(values: NArray[Double]): HSL = dimensionCheck(values, 3).asInstanceOf[HSL]

    def clamp(values: NArray[Double]): HSL = {
      dimensionCheck(values, 3)
      clamp(values(0), values(1), values(2))
    }

    /**
     * HSL is the primary case class for representing colors in HSL space.
     *
     * @constructor Create a new HSV object from three Double values.  This constructor does not validate input parameters.
     *              For values taken from user input, sensors, or otherwise uncertain sources, consider using the factory method in the Color companion object.
     * @see [[ai.dragonfly.color.ColorVector.hsl]] for a method of constructing HSL objects that validates inputs.
     * @see [[https://en.wikipedia.org/wiki/HSL_and_HSV]] for more information about the HSL color space.
     * @param hue        an angle ranging from [0-360] degrees.  Values outside of this range may cause errors.
     * @param saturation a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @param lightness  a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @return an instance of the HSL case class.
     * @example {{{
     * val c = HSL(211f, 75f, 33.3333f)
     * c.toString()  // returns "HSL(211.000,75.000,33.333)"
     * }}}
     */
    def apply(hue: Double, saturation: Double, lightness: Double): HSL = {
      NArray[Double](hue, saturation, lightness).asInstanceOf[HSL]
    }

    def clamp(hue: Double, saturation: Double, lightness: Double): HSL = NArray[Double](
      clampHue(hue),
      clamp0to1(saturation),
      clamp0to1(lightness)
    ).asInstanceOf[HSL]

    def fromRGB(nrgb: RGB): HSL = toHSL(nrgb.red, nrgb.green, nrgb.blue)

    /**
     * Factory method for creating instances of the HSL class.  This method validates input parameters and throws an exception
     * if one or more of them lie outside of their allowed ranges.
     *
     * @param saturation an angle ranging from [0-360] degrees.
     * @param hue        a percentage ranging from [0-100].
     * @param lightness  a percentage ranging from [0-100].
     * @return an instance of the HSL case class.
     */
    def getIfValid(hue: Double, saturation: Double, lightness: Double): Option[HSL] = {
      if (validHue(hue) && valid0to1(saturation) && valid0to1(lightness)) Some(apply(hue, saturation, lightness))
      else None
    }

    inline def toHSL(red: Double, green: Double, blue: Double): HSL = {
      val values: NArray[Double] = hueMinMax(red, green, blue)

      val delta: Double = values(2 /*MAX*/) - values(1 /*min*/)
      val L: Double = (values(1 /*min*/) + values(2 /*MAX*/))

      values(1) = if (delta == 0.0) 0.0 else delta / (1.0 - Math.abs((L) - 1.0))
      values(2) = 0.5 * L // (min + max) / 2
      values.asInstanceOf[HSL]
    }

    override def random(r: scala.util.Random = Random.defaultRandom): HSL = apply(
      NArray[Double](
        r.nextDouble() * 360.0,
        r.nextDouble(),
        r.nextDouble()
      )
    )

    override def toVec(hsl: HSL): Vec[3] = Vec[3](
      hsl(1) * Math.cos(slash.degreesToRadians(hsl(0))),
      hsl(1) * Math.sin(slash.degreesToRadians(hsl(0))),
      hsl(2)
    )

    def hue(hsl: HSL): Double = hsl(0)

    def saturation(hsl: HSL): Double = hsl(1)

    def lightness(hsl: HSL): Double = hsl(2)

    override def toRGB(c: HSL): RGB = c.toRGB

    override def toXYZ(c: HSL): XYZ = c.toXYZ

    override lazy val usableGamut: Gamut = {
      new Gamut( Cylinder(sideSegments = 64) )
    }
  }

  type HSL = HSL.HSL

  given CylindricalColorModel[HSL] with {
    extension (hsl: HSL) {
  //case class HSL private(override val values: NArray[Double]) extends HueSaturation[HSL] {

      def hue: Double = HSL.hue(hsl)

      def saturation: Double = HSL.saturation(hsl)

      def lightness: Double = HSL.lightness(hsl)

      override def similarity(that: HSL): Double = HSL.similarity(hsl, that)

      override def copy: HSL = NArray[Double](hue, saturation, lightness).asInstanceOf[HSL]

      def toRGB: RGB = {
        // https://www.rapidtables.com/convert/color/hsl-to-rgb.html
        val C = (1.0 - Math.abs((2 * lightness) - 1.0)) * saturation
        RGB.apply(
          HSL.hcxmToRGBvalues(
            hue,
            C,
            HSL.XfromHueC(hue, C), // X
            lightness - (0.5 * C) // m
          )
        )
      }

      override def toXYZ: XYZ = toRGB.toXYZ

      override def render: String = s"HSL($hue, $saturation, $lightness)"

      /**
       * @return a string representing the color in an SVG friendly way.
       * @example {{{
       * val c = HSL(211f, 75f, 33.3333f)
       * c.svg() // returns "hsl(211.000,75.0%,33.3%)"
       * }}}
       */
      def svg(): String = s"hsl(${f"$hue%1.3f"}, ${f"$saturation%1.1f"}%, ${f"$lightness%1.1f"}%)"
    }
  }
}
