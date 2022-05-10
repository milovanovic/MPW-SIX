// SPDX-License-Identifier: Apache-2.0

package spectrometer_v2

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.{FlatSpec, Matchers}

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class SpectrometerTestSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  
  val fftSize = sys.props.getOrElse("fftSize", "512")
  val minSRAMDepth = sys.props.getOrElse("minSRAMDepth", "512")
  val enablePlot = sys.props.getOrElse("enablePlot", "false")

  val simulator = "verilator" //"treadle" //"verilator"

  val params = (new SpectrometerParams(fftSize.toInt, minSRAMDepth.toInt)).params

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  Chain Test
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test AXI4Spectrometer" in {
    val lazyDut = LazyModule(new AXI4Spectrometer(params, 4) with AXI4SpectrometerPins)
    chisel3.iotesters.Driver.execute(Array("--backend-name", simulator, "--target-dir", "test_run_dir/AXI4Spectrometer", "--top-name", "AXI4Spectrometer"), () => lazyDut.module) {
      c => new SpectrometerTester(lazyDut, params, fftSize.toInt, false, enablePlot.toBoolean, true)
    } should be (true)
  }
}
