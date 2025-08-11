package ai.dragonfly.uriel.color.model.perceptual

import narr.*
import ai.dragonfly.uriel.cie.WorkingSpace
import ai.dragonfly.uriel.color.spectral.DEFAULT
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import slash.vector.*
import slash.matrix.*

import scala.language.implicitConversions

/**
 * From: https://en.wikipedia.org/wiki/CIE_1931_color_space
 * "The CIE XYZ color space encompasses all color sensations that are visible to a person with average eyesight.
 * That is why CIE XYZ (Tristimulus values) is a device-invariant representation of color."
 *
 * "In the CIE 1931 model, Y is the luminance, Z is quasi-equal to blue (of CIE RGB), and X is a mix of the three CIE RGB
 * curves chosen to be nonnegative."
 *
 * "... the Z value is solely made up of the S cone response, the Y value a mix of L and M responses, and X value a mix
 * of all three. This fact makes XYZ values analogous to, but different from, the LMS cone responses of the human eye."
 */

trait XYZ { self:WorkingSpace =>

  object XYZ extends PerceptualSpace[XYZ] {

    opaque type XYZ = Vec[3]

    override lazy val fullGamut: Gamut = Gamut.fromSpectralSamples(cmf, illuminant)

    override lazy val usableGamut: Gamut = Gamut.fromRGB(transform = (xyz:XYZ) => xyz.asInstanceOf[Vec[3]])

    def apply(values: NArray[Double]): XYZ = dimensionCheck(values, 3).asInstanceOf[XYZ]

    def apply(x: Double, y: Double, z: Double): XYZ = Vec[3](x, y, z)

    def x(xyz: XYZ): Double = xyz(0)

    def y(xyz: XYZ): Double = xyz(1)

    def z(xyz: XYZ): Double = xyz(2)

    override def toRGB(xyz: XYZ): RGB = {
      val temp: NArray[Double] = (M_inverse * xyz.asColumnMatrix).values
      var i: Int = 0;
      while (i < temp.length) {
        temp(i) = transferFunction.encode(temp(i))
        i += 1
      }
      RGB(temp)
    }

    override def toXYZ(c: XYZ): XYZ = c.copy
    override def fromXYZ(xyz: XYZ): XYZ = xyz.asInstanceOf[Vec[3]].copy

    def copy(xyz:XYZ):XYZ = xyz.copy

    override def fromVec(v: Vec[3]): XYZ = v

    override def toVec(xyz: XYZ): Vec[3] = xyz.asInstanceOf[Vec[3]].copy


  }

  type XYZ = XYZ.XYZ

  given PerceptualColorModel[XYZ] with { //case class XYZ private(override val values: NArray[Double]) extends PerceptualColorModel[XYZ] {
    extension (xyz: XYZ) {
      override inline def copy: XYZ = XYZ.copy(xyz)

      def x: Double = XYZ.x(xyz)

      def y: Double = XYZ.y(xyz)

      def z: Double = XYZ.z(xyz)

      override def similarity(that: XYZ): Double = XYZ.similarity(xyz, that)

      override def toXYZ: XYZ = XYZ(x, y, z)

      override def vec:Vec[3] = XYZ.asInstanceOf[Vec[3]].copy

      override def toRGB: RGB = XYZ.toRGB(xyz)

      override def render: String = s"XYZ($x,$y,$z)"

    }
  }
}
