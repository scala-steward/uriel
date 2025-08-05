package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.spectral.*
import ai.dragonfly.bitfrost.visualization.*
import ai.dragonfly.mesh.sRGB
import ai.dragonfly.mesh.io.PLY
import slash.vector.*

import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream}
import javax.imageio.ImageIO
import scala.language.implicitConversions

object ColorSpaceNoise extends App {

  println("Starting ColorSpaceNoise")

  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  val contexts = ai.dragonfly.bitfrost.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    println(s"\t$context:")

    val GMG: GamutMeshGenerator = GamutMeshGenerator(context)

    def noisyImage[C:ColorModel](space:Space[C], transform: Vec[3] => ColorContext.sRGB.ARGB32):Unit = {

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          val c = space.random()
          bi.setRGB(x, y, transform(c.toXYZ.asInstanceOf[Vec[3]]))
        }
      }

      val fileName = s"./demo/image/$context$space.png"
      if (ImageIO.write(bi, "PNG", new File(fileName))) println(s"\t\tWrote $fileName")
      else println(s"\t\tFailed to write $fileName")
    }

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

    noisyImage(RGB, GMG.XYZtoARGB32)
    noisyImage(CMY, GMG.XYZtoARGB32)
    noisyImage(HSV, GMG.XYZtoARGB32)
    noisyImage(HSL, GMG.XYZtoARGB32)
    noisyImage(XYZ, GMG.XYZtoARGB32)
    noisyImage(Lab, GMG.XYZtoARGB32)
    noisyImage(Luv, GMG.XYZtoARGB32)

  }

}
