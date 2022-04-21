// SPDX-License-Identifier: Apache-2.0

package spectrometer

import dsptools.numbers._

import chisel3._
import chisel3.experimental._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import fft._
import magnitude._
import accumulator._


case class SpectrometerVanillaParameters (
  fftParams       : FFTParams[FixedPoint],
  magParams       : MAGParams[FixedPoint],
  accParams       : AccParams[FixedPoint],
  fftAddress      : AddressSet,
  magAddress      : AddressSet,
  accAddress      : AddressSet,
  accQueueBase    : BigInt,
  beatBytes       : Int
)

class SpectrometerVanilla(params: SpectrometerVanillaParameters) extends LazyModule()(Parameters.empty) {

  val fft = LazyModule(new AXI4FFTBlock(address = params.fftAddress, params = params.fftParams, _beatBytes = params.beatBytes, configInterface = false))
  val mag = LazyModule(new AXI4LogMagMuxBlock(params.magParams, params.magAddress, _beatBytes = params.beatBytes))
  val acc = LazyModule(new AXI4AccChainBlock(params.accParams, params.accAddress, params.accQueueBase, params.beatBytes))

  val streamNode = NodeHandle(fft.streamNode, acc.streamNode)

  // define mem
  lazy val blocks = Seq(fft, mag)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }
  acc.mem.get := bus.node

  // connect nodes
  acc.streamNode := mag.streamNode := fft.streamNode
  
  lazy val module = new LazyModuleImp(this) {}
}

trait SpectrometerVanillaPins extends SpectrometerVanilla {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  // streamNode
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}


class SpectrometerVanillaParams(fftSize: Int = 1024) {
  val params = SpectrometerVanillaParameters (
    fftParams = FFTParams.fixed(
      dataWidth = 16,
      twiddleWidth = 16,
      numPoints = fftSize,
      useBitReverse  = true,
      runTime = false,
      numAddPipes = 1,
      numMulPipes = 1,
      use4Muls = true,
      //sdfRadix = "2",
      expandLogic = Array.fill(log2Up(fftSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
      keepMSBorLSB = Array.fill(log2Up(fftSize))(true),
      minSRAMdepth = fftSize, // memories larger than 64 should be mapped on block ram
      binPoint = 10
    ),
    magParams = MAGParams(
      protoIn  = FixedPoint(16.W, 10.BP),
      protoOut = FixedPoint(16.W, 10.BP),
      protoLog = Some(FixedPoint(16.W, 10.BP)),
      magType  = MagJPLandSqrMag,
      log2LookUpWidth = 10,
      useLast = true,
      numAddPipes = 1,
      numMulPipes = 1
    ),
    accParams = AccParams(
      proto    = FixedPoint(16.W, 10.BP),
      protoAcc = FixedPoint(32.W, 10.BP),
    ),
    fftAddress      = AddressSet(0x30000100, 0xFF),
    magAddress      = AddressSet(0x30000200, 0xFF),
    accAddress      = AddressSet(0x30000310, 0xF),
    accQueueBase    = 0x30002000,
    beatBytes      = 4)
}
object SpectrometerVanillaApp extends App
{
  // here just define parameters
  val params = (new SpectrometerVanillaParams).params

  implicit val p: Parameters = Parameters.empty
  val standaloneModule = LazyModule(new SpectrometerVanilla(params) with SpectrometerVanillaPins) 
  chisel3.Driver.execute(Array("--target-dir", "verilog/SpectrometerVanilla", "--top-name", "SpectrometerVanilla"), ()=> standaloneModule.module) // generate verilog code
}

