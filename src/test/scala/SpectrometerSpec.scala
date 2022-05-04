// SPDX-License-Identifier: Apache-2.0

package spectrometer_v2

import chisel3._
import chisel3.util._
import chisel3.iotesters.{Driver, PeekPokeTester}

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.{FlatSpec, Matchers}

import java.io._

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class SpectrometerTestSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  
  val fftSize = sys.props.getOrElse("fftSize", "512")
  val minSRAMDepth = sys.props.getOrElse("minSRAMDepth", "512")
  val enablePlot = sys.props.getOrElse("enablePlot", "false")
  val params = (new SpectrometerParams(fftSize.toInt, minSRAMDepth.toInt)).params

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  Chain Test
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test AXI4Spectrometer" in {
    val lazyDut = LazyModule(new AXI4Spectrometer(params, 4) with AXI4SpectrometerPins)
    chisel3.iotesters.Driver.execute(Array("--backend-name", "verilator", "--target-dir", "test_run_dir/SpectrometerTest/pin_fft_mag_pout", "--top-name", "SpectrometerTest"), () => lazyDut.module) {
      c => new SpectrometerTester(lazyDut, params,  enablePlot.toBoolean, true)
    } should be (true)
  }
