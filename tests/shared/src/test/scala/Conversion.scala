import ai.dragonfly.uriel.ColorContext

class Conversion extends munit.FunSuite {
  test("Conversion") {
    val ctx = ColorContext.sRGB
    import ctx.*
    val c1:RGBA32 = RGBA32(127, 255, 64)
  }
}
