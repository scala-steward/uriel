/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.uriel.cie

import narr.*
import slash.vector.*

import slash.matrix
import matrix.*

import scala.language.implicitConversions

object ChromaticAdaptation {

  // Chromatic Adaptation Matrices from  http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

  lazy val XYZ_Scaling: Mat[3,3] = Mat.identity[3,3]
  lazy val XYZ_Scaling_Inverse: Mat[3,3] = XYZ_Scaling

  lazy val Bradford: Mat[3,3] = Mat[3,3](
    0.8951, 0.2664, -0.1614,
    -0.7502, 1.7135, 0.0367,
    0.0389, -0.0685, 1.0296
  )

  lazy val Bradford_Inverse: Mat[3,3] = Mat[3,3](
    0.9869929, -0.1470543, 0.1599627,
    0.4323053, 0.5183603, 0.0492912,
    -0.0085287, 0.0400428, 0.9684867
  )

  lazy val Von_Kries: Mat[3,3] = Mat[3,3](
    0.40024, 0.7076, -0.08081,
    -0.2263, 1.16532, 0.0457,
    0.0, 0.0, 0.91822
  )
  lazy val Von_Kries_Inverse: Mat[3,3] = Mat[3,3](
    1.8599364, -1.1293816, 0.2198974,
    0.3611914, 0.6388125, -0.0000064,
    0.0, 0.0, 1.0890636
  )

}

case class ChromaticAdaptation[S <: WorkingSpace, T <: WorkingSpace](source:S, target:T, m:Mat[3,3] = Bradford) {

  val s:NArray[Double] = (m * source.illuminant.asColumnMatrix).values

  val t:NArray[Double] = (m * target.illuminant.asColumnMatrix).values

  val M:Mat[3,3] = m.inverse.times(
    Mat[3,3](
      t(0) / s(0), 0.0, 0.0,
      0.0, t(1) / s(1), 0.0,
      0.0, 0.0, t(2) / s(2)
    ).times(m)
  )

  def apply(xyz:source.XYZ):target.XYZ = target.XYZ((M * (xyz.asInstanceOf[Vec[3]]).asColumnMatrix).values)
}