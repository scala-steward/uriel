package ai.dragonfly.uriel.cie

import narr.*
import ai.dragonfly.uriel.color.spectral.SampleSet

import slash.matrix.ml.unsupervised.dimreduction.PCA
import slash.matrix.ml.data.*

import slash.stats.probability.distributions.Sampleable
import slash.stats.probability.distributions.stream.StreamingVectorStats
import slash.vector.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import slash.geometry.Tetrahedron

import scala.collection.mutable

trait Gamut { self: WorkingSpace =>
  object Gamut {

    def computeMaxDistSquared(points: NArray[Vec[3]], mean: Vec[3]): Double = {

      val vs: NArray[Vec[3]] = NArray.ofSize[Vec[3]](points.length)
      var i:Int = 0; while (i < points.length) {
        vs(i) = points(i) - mean
        i += 1
      }

      val vecSpace = VectorSpace(points.length)

      val pca = PCA(new StaticUnsupervisedData[vecSpace.N, 3](vs))

      val mode = pca.basisPairs.head.basisVector

      var min: Double = Double.MaxValue
      var minV: Vec[3] = mean
      var MAX: Double = Double.MinValue
      var vMAX: Vec[3] = mean

      points.foreach {
        p =>
          val t: Double = mode dot p
          if (t < min) {
            min = t
            minV = p
          }
          if (t > MAX) {
            MAX = t
            vMAX = p
          }
      }

      minV.euclideanDistanceSquaredTo(vMAX)

    }

    def fromRGB(n: Int = 32, transform: XYZ => Vec[3] = (xyz: XYZ) => xyz.vec): Gamut = {

      val m1: Mesh = Cube(1.0, n)

//      val m2: Mesh = Mesh(
//        NArray.tabulate[Vec[3]](m1.points.length)((i:Int) => m1.points(i) ), //* 255.0}),
//        m1.triangles
//      )

      val m3: Mesh = Mesh(
        m1.points.map((vRGB:Vec[3]) => transform(RGB.fromVec(vRGB).toXYZ)),
        m1.triangles
      )

//      val sg:Gaussian = Gaussian()
//
//      var i:Int = 0; while (i < m1.triangles.length) {
//        val t:Triangle = m1.triangles(i)
//        sg.observe( Math.sqrt( t.area(m3.points) / t.area(m2.points) ) )
//        i += 1
//      }
//
//      println(s"$ctx triangle stretch stats: ${sg.estimate}")

      new Gamut( m3 )
    }

    def fromSpectralSamples(spectralSamples: SampleSet, illuminant: Illuminant): Gamut = fromSpectralSamples(
      spectralSamples,
      (v: Vec[3]) => Vec[3](
        v.x * illuminant.xₙ,
        v.y * illuminant.yₙ,
        v.z * illuminant.zₙ,
      )
    )


    def fromSpectralSamples(spectralSamples: SampleSet, transform: Vec[3] => Vec[3] = (v: Vec[3]) => v): Gamut = {

      val points: NArray[Vec[3]] = NArray.tabulate[Vec[3]](spectralSamples.volumePoints.length)(
        (i:Int)=> transform(spectralSamples.volumePoints(i))
      )

      val triangles:mutable.HashSet[Triangle] = mutable.HashSet[Triangle]()

      var t:Int = 0
      def addTriangle(pi0:Int, pi1:Int, pi2:Int): Unit = {
        if (Triangle.nonZeroArea(points(pi0), points(pi1), points(pi2))) {
          triangles += Triangle(pi2, pi1, pi0)
        }
        t += 1
      }

      //addTriangle(Tetrahedron(mean, points(0), points(1), points(spectralSamples.sampleCount)))
      addTriangle(0, 1, spectralSamples.sampleCount)

      // black adjacent:
      while (t < spectralSamples.sampleCount) addTriangle(0, t + 1, t)

      val hEnd: Int = points.length - spectralSamples.sampleCount

      val end = (2 * (points.length - 1)) - spectralSamples.sampleCount
      while (t < end) {
        val i: Int = (t - spectralSamples.sampleCount) / 2
        if (i < hEnd) {
          val h: Int = i + spectralSamples.sampleCount
          if (t % 2 == 1) addTriangle(i, h, h - 1) // Tetrahedron(mean, points(i), points(h), points(h - 1))
          else addTriangle(i+1, h, i) // Tetrahedron(mean, points(i + 1), points(h), points(i))
        } else {
          val h: Int = points.length - 1
          addTriangle(i, h, h - 1) // Tetrahedron(mean, points(i), points(h), points(h - 1))
        }
      }

      // white adjacent:
      for (i <- (points.length - 1) - spectralSamples.sampleCount until points.length - 2) addTriangle(i, i + 1, points.length - 1)

      new Gamut(Mesh.fromPointsAndHashSet(points, triangles, "Spectral Samples Gamut"))
    }

//    println("defined Gamut object methods")
  }


  /**
   *
   * @param tetrahedra
   * @param cumulative
   */

  case class Gamut (volumeMesh:Mesh) extends Sampleable[Vec[3]] {

    val mean: Vec[3] = {
      val sv2:StreamingVectorStats[3] = new StreamingVectorStats[3]()
      volumeMesh.points.foreach((p:Vec[3]) => sv2(p))
      sv2.average()
    }

    val maxDistSquared: Double = Gamut.computeMaxDistSquared(volumeMesh.points, mean)

    val tetrahedra: NArray[Tetrahedron] = NArray.tabulate[Tetrahedron](volumeMesh.triangles.length)((i:Int) => {
      val t: Triangle = volumeMesh.triangles(i)
      Tetrahedron(
        mean,
        volumeMesh.points(t.v1),
        volumeMesh.points(t.v2),
        volumeMesh.points(t.v3)
      )
    })

    val cumulative: NArray[Double] = {
      var totalVolume: Double = 0.0
      val ca:NArray[Double] = NArray.tabulate[Double](volumeMesh.triangles.length)((i:Int) => {
        totalVolume += tetrahedra(i).volume
        totalVolume
      })
      var i:Int = 0; while (i < ca.length) {
        ca(i) = ca(i) / totalVolume
        i += 1
      }
      ca
    }

    private def getNearestIndex(target: Double): Int = {
      var left = 0
      var right = cumulative.length - 1
      while (left < right) {
        val mid = (left + right) / 2
        if (cumulative(mid) < target) left = mid + 1
        else if (cumulative(mid) > target) right = mid - 1
        else return mid
      }
      right
    }

    override def random(r: scala.util.Random = slash.Random.defaultRandom): Vec[3] = {
      val x = r.nextDouble()
      val i = getNearestIndex(x)
      if (i < 0 || i > tetrahedra.length) println(s"x = $x, i = $i, cumulative.length = ${cumulative.length}")
      tetrahedra(i).random(r)
    }

  }
}