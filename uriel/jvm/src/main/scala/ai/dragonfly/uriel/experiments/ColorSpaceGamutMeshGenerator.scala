package ai.dragonfly.uriel.experiments

import slash.vector.Vec

import ai.dragonfly.mesh.sRGB
import ai.dragonfly.mesh.io.PLY

import java.io.File

object ColorSpaceGamutMeshGenerator extends App {

  println("Starting ColorSpaceGamutMeshGenerator")

  val contexts = ai.dragonfly.uriel.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    println(s"Context \t$context:")

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

    writeMesh(XYZ.fullGamut, "XYZ_Full", (cv:Vec[3]) => Gamut.XYZtoARGB32(cv).asInstanceOf[sRGB.ARGB32])
    writeMesh(XYZ.usableGamut, "XYZ_Usable", (cv:Vec[3]) => Gamut.XYZtoARGB32(cv).asInstanceOf[sRGB.ARGB32])

    writeMesh(Lab.fullGamut, "Lab_Full", (cv:Vec[3]) => Gamut.XYZtoARGB32(Lab.toXYZ(Lab.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])
    writeMesh(Lab.usableGamut, "Lab_Usable", (cv:Vec[3]) => Gamut.XYZtoARGB32(Lab.toXYZ(Lab.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])

    writeMesh(Luv.fullGamut, "Luv_Full", (cv:Vec[3]) => Gamut.XYZtoARGB32(Luv.toXYZ(Luv.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])
    writeMesh(Luv.usableGamut, "Luv_Usable", (cv:Vec[3]) => Gamut.XYZtoARGB32(Luv.toXYZ(Luv.fromVec(cv)).asInstanceOf[Vec[3]]).asInstanceOf[sRGB.ARGB32])

  }

}