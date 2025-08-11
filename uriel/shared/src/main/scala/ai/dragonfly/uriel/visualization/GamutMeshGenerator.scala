package ai.dragonfly.uriel.visualization

import ai.dragonfly.uriel.ColorContext
import ai.dragonfly.uriel.ColorContext.sRGB.ColorModel
import ai.dragonfly.uriel.cie.{ChromaticAdaptation, WorkingSpace}

import ai.dragonfly.uriel.color.model.huesat.*

import slash.vector.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*

class GamutMeshGenerator(val ws:WorkingSpace) {

  lazy val fullGamutXYZ:ws.Gamut = ws.Gamut.fromSpectralSamples(
    ws.cmf,
    (v:Vec[3]) => Vec[3](
      ws.whitePoint.x * v.x,
      ws.whitePoint.y * v.y,
      ws.whitePoint.z * v.z
    )
  )

  val XYZtoARGB32:Vec[3] => ColorContext.sRGB.ARGB32 = {
    import ColorContext.sRGB
    if (ws == sRGB.ARGB32) {
      (v: Vec[3]) => sRGB.ARGB32.fromXYZ(sRGB.XYZ(v.asNativeArray))
    } else {
      val chromaticAdapter: ChromaticAdaptation[ws.type, sRGB.type] = ChromaticAdaptation[ws.type, sRGB.type](ws, sRGB)
      (v: Vec[3]) => sRGB.ARGB32.fromXYZ(chromaticAdapter(ws.XYZ(v.asNativeArray)))
    }
  }

  def fullGamut[C:ColorModel](space:WorkingSpace#PerceptualSpace[C]):ColorGamutVolumeMesh = space match {
    case _:ws.XYZ.type => ColorGamutVolumeMesh( fullGamutXYZ.volumeMesh, XYZtoARGB32 )
    case wsSpace:ws.PerceptualSpace[C] => ColorGamutVolumeMesh(
        fullGamutXYZ.volumeMesh.transform(
          (v: Vec[3]) => wsSpace.toVec(wsSpace.fromXYZ(ws.XYZ.fromVec(v)))
        ),
        (v:Vec[3]) => XYZtoARGB32(space.fromVec(v).toXYZ.asInstanceOf[Vec[3]])
      )
    case _ => throw Exception("Wrong Working ColorSpace!")
  }

  def usableGamut[C:ColorModel](space:WorkingSpace#VectorSpace[C]):ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    space match {
      case perceptualSpace: ws.PerceptualSpace[C] => perceptualSpace.usableGamut.volumeMesh
      case _: ws.CylindricalSpace[C] =>
        try {
          if (space == ws.asInstanceOf[HSL].HSL) Cylinder(sideSegments = 64)
          else if (space == ws.asInstanceOf[HSV].HSV) Cylinder(capSegments = 6)
          else throw Exception("Kick the can down the road!")
        } catch {
          case _ =>
            if (space == ws.asInstanceOf[HSV].HSV) Cylinder(capSegments = 6)
            else throw Exception("Kick the can down the road!")
        }
      case _: ws.VectorSpace[C] => Cube()
    },
    (v:Vec[3]) => XYZtoARGB32((space.fromVec(v)).toXYZ.asInstanceOf[Vec[3]])
  )

}

case class ColorGamutVolumeMesh(mesh:Mesh, vertexColorMapper:Vec[3] => ColorContext.sRGB.ARGB32)