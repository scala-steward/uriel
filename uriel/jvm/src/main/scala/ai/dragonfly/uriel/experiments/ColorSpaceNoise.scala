package ai.dragonfly.uriel.experiments

import ai.dragonfly.uriel.ColorContext



import ai.dragonfly.uriel.visualization.*


import slash.vector.*

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.language.implicitConversions

object ColorSpaceNoise extends App {

  println("Starting ColorSpaceNoise")

  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  val contexts = ai.dragonfly.uriel.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    println(s"\t$context:")

    val GMG: GamutMeshGenerator = GamutMeshGenerator(context)

    def noisyImage[C:ColorModel](space:ColorSpace[C]):Unit = {

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          val c = space.random()
          bi.setRGB(x, y, GMG.XYZtoARGB32(c.toXYZ.asInstanceOf[Vec[3]]))
        }
      }

      val fileName = s"./demo/image/$context$space.png"
      if (ImageIO.write(bi, "PNG", new File(fileName))) println(s"\t\tWrote $fileName")
      else println(s"\t\tFailed to write $fileName")
    }

    noisyImage(RGB)
    noisyImage(CMY)
    noisyImage(HSV)
    noisyImage(HSL)
    noisyImage(XYZ)
    noisyImage(Lab)
    noisyImage(Luv)


//    for (space <- Seq(XYZ, RGB, CMY, CMYK, Lab, Luv, HSV, HSL)) { // ARGB32, RGBA32, ARGB64, RGBA64)) { //
//      space match {
//        case perceptualSpace: GMG.ws.PerceptualSpace[_] =>
//          val fg: ColorGamutVolumeMesh = GMG.fullGamut[](perceptualSpace)
//          PLY.writeMesh(
//            fg.mesh,
//            new java.io.FileOutputStream( new File(s"./demo/ply/$context${perceptualSpace}FullGamut.ply" ) ),
//            (cv:Vec[3]) => sRGB.ARGB32(fg.vertexColorMapper(cv))
//          )
//        case _ =>
//      }
//
//      val ug: ColorGamutVolumeMesh = GMG.usableGamut(space)
//      PLY.writeMesh(
//        ug.mesh,
//        new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") ),
//        (cv:Vec[3]) => sRGB.ARGB32(ug.vertexColorMapper(cv))
//      )
//    }

  }

}
