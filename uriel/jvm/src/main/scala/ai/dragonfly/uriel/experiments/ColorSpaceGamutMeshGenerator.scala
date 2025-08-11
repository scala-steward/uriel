package ai.dragonfly.uriel.experiments

import ai.dragonfly.uriel.visualization.GamutMeshGenerator
import slash.vector.Vec

import ai.dragonfly.mesh.sRGB
import ai.dragonfly.mesh.io.PLY

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ColorSpaceGamutMeshGenerator extends App {

  println("Starting ColorSpaceGamutMeshGenerator")

  val contexts = ai.dragonfly.uriel.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    println(s"Context \t$context:")

    val GMG: GamutMeshGenerator = GamutMeshGenerator(context)

    def writeMesh(g: Gamut, name: String, toARGB: Vec[3] => sRGB.ARGB32 ):Unit = {
      PLY.writeMesh(
        g.volumeMesh,
        new java.io.FileOutputStream(new File(s"./demo/ply/$context${name}Gamut.ply")),
        toARGB
      )
    }

    writeMesh(RGB.usableGamut, "RGB_Usable", (cv:Vec[3]) => ARGB32.fromRGB(RGB.fromVec(cv)).asInstanceOf[sRGB.ARGB32])

    writeMesh(CMY.usableGamut, "CMY_Usable", (cv:Vec[3]) => ARGB32.fromRGB(CMY.fromVec(cv).toRGB).asInstanceOf[sRGB.ARGB32])
    writeMesh(CMYK.usableGamut, "CMYK_Usable", (cv:Vec[3]) => ARGB32.fromRGB(CMYK.fromVec(cv).toRGB).asInstanceOf[sRGB.ARGB32])

    writeMesh(HSL.usableGamut, "HSL_Usable", (cv:Vec[3]) => ARGB32.fromRGB(HSL.fromVec(cv).toRGB).asInstanceOf[sRGB.ARGB32])
    writeMesh(HSV.usableGamut, "HSV_Usable", (cv:Vec[3]) => ARGB32.fromRGB(HSV.fromVec(cv).toRGB).asInstanceOf[sRGB.ARGB32])

    writeMesh(XYZ.fullGamut, "XYZ_Full", (cv:Vec[3]) => GMG.XYZtoARGB32(cv).asInstanceOf[sRGB.ARGB32])
    writeMesh(XYZ.usableGamut, "XYZ_Usable", (cv:Vec[3]) => GMG.XYZtoARGB32(cv).asInstanceOf[sRGB.ARGB32])

    writeMesh(Lab.fullGamut, "Lab_Full", (cv:Vec[3]) => GMG.XYZtoARGB32(Lab.toXYZ(Lab.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])
    writeMesh(Lab.usableGamut, "Lab_Usable", (cv:Vec[3]) => GMG.XYZtoARGB32(Lab.toXYZ(Lab.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])

    writeMesh(Luv.fullGamut, "Luv_Full", (cv:Vec[3]) => GMG.XYZtoARGB32(Luv.toXYZ(Luv.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])
    writeMesh(Luv.usableGamut, "Luv_Usable", (cv:Vec[3]) => GMG.XYZtoARGB32(Luv.toXYZ(Luv.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])

//    for (space <- Seq(RGB, CMY, CMYK, HSV, HSL, Lab, Luv)) { // ARGB32, RGBA32, ARGB64, RGBA64)) { //
//      println(s"Space: $space")
//      space match {
//        case ps: context.PerceptualSpace[_] =>
//          println("Perceptal")
//          val fg: context.Gamut = ps.fullGamut
//          println(s"Gamut $fg")
//          PLY.writeMesh(
//            fg.volumeMesh,
//            new java.io.FileOutputStream( new File(s"./demo/ply/$context${ps}FullGamut.ply" ) ),
//            (cv:Vec[3]) => GMG.XYZtoARGB32(ps.toXYZ(ps.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32]
//          )
//        case _ =>
//      }
//
//      val ug: context.Gamut = space.usableGamut
//      PLY.writeMesh(
//        ug.volumeMesh,
//        new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") ),
//        (cv:Vec[3]) => GMG.XYZtoARGB32(space.toXYZ(space.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32]
//      )
//    }

  }

}