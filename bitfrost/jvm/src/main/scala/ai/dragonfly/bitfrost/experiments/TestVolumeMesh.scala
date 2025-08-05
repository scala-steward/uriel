package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.mesh.io.PLY
import ai.dragonfly.bitfrost.visualization.ColorGamutVolumeMesh
import slash.vector.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*

import java.io.File

import scala.language.implicitConversions

object TestVolumeMesh extends App {

  import ai.dragonfly.mesh.Mesh.*
  import ai.dragonfly.bitfrost.ColorContext.sRGB.*
  import ai.dragonfly.bitfrost.ColorContext.sRGB.RGB.*
  import ai.dragonfly.bitfrost.ColorContext.sRGB.ARGB32.*

  val sRGB_RGB_TestCube: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cube(),
    (v: Vec[3]) => ARGB32.fromRGB(v.asInstanceOf[RGB])
  )
  PLY.writeMesh(
    sRGB_RGB_TestCube.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_RGB_TestCube.ply")),
    (c:Vec[3]) => sRGB.ARGB32(sRGB_RGB_TestCube.vertexColorMapper(c))
  )

  val sRGB_HSV_TestCylinder: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cylinder(capSegments = 4),
    (v: Vec[3]) => ARGB32.fromRGB(HSV.fromVec(v).toRGB)
  )

  PLY.writeMesh(
    sRGB_HSV_TestCylinder.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSV_TestCylinder.ply") ),
    (c:Vec[3]) => sRGB.ARGB32(sRGB_HSV_TestCylinder.vertexColorMapper(c))
  )

  val sRGB_HSL_TestCylinder: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cylinder(sideSegments = 10),
    (v:Vec[3]) => ARGB32.fromRGB(HSL.fromVec(v).toRGB)
  )
  PLY.writeMesh(
    sRGB_HSL_TestCylinder.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSL_TestCylinder.ply") ),
    (c:Vec[3]) => sRGB.ARGB32(sRGB_HSL_TestCylinder.vertexColorMapper(c))
  )

}