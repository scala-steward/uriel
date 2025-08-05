package ai.dragonfly.bitfrost.color.spectral

import slash.squareInPlace
import slash.vector.*
import narr.*

trait SampleSet {

  def samples: NArray[Sample]

  def sampleCount: Int = samples.length

  lazy val volumePoints:NArray[Vec[3]] = {

    //val points: NArray[Vec[3]] = new NArray[Vec[3]](squareInPlace(samples.length))
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

//    println(s"${points.length} vs $p")
//
//    // normalize:
//    for (i <- points.indices) {
//      val xyz: Vec[3] = points(i)
//      points(i) =  Vec[3](
//        xyz.x / sv.maxValues(0),
//        xyz.y / sv.maxValues(1),
//        xyz.z / sv.maxValues(2)
//      )
//    }

    points
  }
}