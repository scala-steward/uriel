package ai.dragonfly.uriel.experiments

import ai.dragonfly.uriel.ColorContext.sRGB
import slash.stats.probability.distributions.stream.StreamingVectorStats

object LuvStats extends App {

  val pts = sRGB.Luv.usableGamut.volumeMesh.points

  val stats: StreamingVectorStats[3] = new StreamingVectorStats[3]()

  var i = 0
  while (i < pts.length) {
    stats(pts(i))
    i = i + 1
  }

  println(stats)
  println(stats.bounds())
}
