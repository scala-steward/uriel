package ai.dragonfly.uriel.cie

import narr.*
import slash.matrix.*

object ChromaticityPrimary {
  def inferThird(cp1: ChromaticityPrimary, cp2: ChromaticityPrimary): ChromaticityPrimary = ChromaticityPrimary(
    1.0 - (cp1.x + cp2.x),
    1.0 - (cp1.y + cp2.y),
    1.0 - (cp1.Y + cp2.Y)
  )
}

/**
 * Given two primaries: the third can be inferred.
 *
 * @param v Primary x, y with x between [0.0, 1.0] and y between [0.0, 1.0]
 * @param Y brightness of the primary
 */

case class ChromaticityPrimary(x:Double, y:Double, Y:Double)

/**
 * Assumes:
 * RED.x + GEEN.x + BLUE.x = 1.0
 * RED.y + GEEN.y + BLUE.y = 1.0
 * RED.Y + GEEN.Y + BLUE.Y = 1.0
 *
 * @param red red chromatic primary
 * @param green green chromatic primary
 * @param blue blue chromatic primary
 */

case class ChromaticityPrimaries(red: ChromaticityPrimary, green: ChromaticityPrimary, blue: ChromaticityPrimary) {

  // from http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html

  def raw(S: NArray[Double] = NArray[Double](1.0, 1.0, 1.0)):NArray[Double] = NArray[Double](
    S(0) * (red.x / red.y), S(1) * (green.x / green.y), S(2) * (blue.x / blue.y),
    S(0), S(1), S(2),
    S(0) * ((1.0 - red.x - red.y) / red.y), S(1) * ((1.0 - green.x - green.y) / green.y), S(2) * ((1.0 - blue.x - blue.y) / blue.y)
  )

  lazy val xyzXrgbInv:Mat[3,3] = Mat[3,3]( raw() ).inverse

  def getM(illuminant: Illuminant):Mat[3,3] = Mat[3,3](raw((xyzXrgbInv * illuminant.asColumnMatrix).values))

}