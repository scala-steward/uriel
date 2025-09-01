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

package ai.dragonfly.uriel.color.spectral


import slash.vector.*
import narr.*

trait SampleSet {

  def samples: NArray[Sample]

  def sampleCount: Int = samples.length

  lazy val volumePoints:NArray[Vec[3]] = {

    val points: NArray[Vec[3]] = NArray.ofSize[Vec[3]](
      2 + (samples.length * (samples.length - 1))
    )

    points(0) = Vec[3](0.0, 0.0, 0.0)

    val `xyzʷ`:Vec[3] = {
      val v: Vec[3] = Vec[3](0.0, 0.0, 0.0)
      for (s <- samples) v.add(s.xyz)
      v
    }

    var p: Int = 1

    points(points.length - 1) = Vec[3](1.0, 1.0, 1.0)

    for (i <- 0 until samples.length - 1) {
      for (j <- 0 until samples.length) {
        val v: Vec[3] = Vec[3](0.0, 0.0, 0.0)
        for (k <- 0 to i) {
          v.add(samples((j + k) % samples.length).xyz)
        }
        points(p) = Vec[3](v.x / `xyzʷ`.x, v.y / `xyzʷ`.y, v.z / `xyzʷ`.z)
        p += 1
      }
    }

    points
  }
}