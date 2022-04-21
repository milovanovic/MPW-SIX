// SPDX-License-Identifier: Apache-2.0

package spectrometer

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._

/* Spectrometer parameters */
case class SpectrometerParameters[T <: Data: Real: BinaryRepresentation] (
  winParams  : Option[WinParamsAndAddresses[T]],
  fftParams  : Option[FFTParamsAndAddresses[T]],
  magParams  : Option[MagParamsAndAddresses[T]],
  accParams  : Option[AccParamsAndAddresses[T]],
  cfarParams : Option[CFARParamsAndAddresses[T]]
)

/* Windows parameters and addresses */
case class WinParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  winParams     : WindowingParams[T],
  winRAMAddress : AddressSet,
  winCSRAddress : AddressSet
)
/* FFT parameters and addresses */
case class FFTParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  fftParams  : FFTParams[T],
  fftAddress : AddressSet
)
/* Magnitude parameters and addresses */
case class MagParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  magParams  : MAGParams[T],
  magAddress : AddressSet
)
/* Accumulator parameters and addresses */
case class AccParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  accParams    : AccParams[T],
  accAddress   : AddressSet,
  accQueueBase : BigInt
)
/* CFAR parameters and addresses */
case class CFARParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  cfarParams  : CFARParams[T],
  cfarAddress : AddressSet
)

class AXI4Spectrometer[T <: Data : Real: BinaryRepresentation](params: SpectrometerParameters[T], beatBytes: Int)(implicit p: Parameters) extends Spectrometer[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  /* Optional memory mapped port */
  val bus = if (blocks.isEmpty) None else Some(LazyModule(new AXI4Xbar))
  override val mem = if (blocks.isEmpty) None else Some(bus.get.node)
  for (b <- blocks) {
    b.mem.foreach { _ := bus.get.node }
  }
}

abstract class Spectrometer [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: SpectrometerParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

  /* Type of Blocks */
  type Block = AXI4DspBlock

  val win  : Option[Block] = if (params.winParams  != None) Some(LazyModule(new WindowingBlock(csrAddress = params.winParams.get.winCSRAddress, ramAddress = params.winParams.get.winRAMAddress, params.winParams.get.winParams, beatBytes = beatBytes))) else None
  val fft  : Option[Block] = if (params.fftParams  != None) Some(LazyModule(new AXI4FFTBlock(address = params.fftParams.get.fftAddress, params = params.fftParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag  : Option[Block] = if (params.magParams  != None) Some(LazyModule(new AXI4LogMagMuxBlock(params.magParams.get.magParams, params.magParams.get.magAddress, _beatBytes = beatBytes))) else None
  val acc  : Option[Block] = if (params.accParams  != None) Some(LazyModule(new AXI4AccChainBlock(params.accParams.get.accParams, params.accParams.get.accAddress, params.accParams.get.accQueueBase, beatBytes))) else None
  val cfar : Option[Block] = if (params.cfarParams != None) Some(LazyModule(new AXI4CFARBlock(params.cfarParams.get.cfarParams, params.cfarParams.get.cfarAddress, _beatBytes = beatBytes))) else None

  /* Blocks */
  val blocks: Seq[Block]  = Seq(win, fft, mag, acc, cfar).flatten
  require(blocks.length >= 1, "At least one block should exist")
  
  /* Connect nodes */
  lazy val connections = for (i <- 1 until blocks.length) yield (blocks(i), blocks(i-1))
  for ((lhs, rhs) <- connections) {
    lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
  }

  /* Optional streamNode */
  val streamNode = NodeHandle(blocks.head.streamNode, blocks.last.streamNode)

  lazy val module = new LazyModuleImp(this) {}
}

trait AXI4SpectrometerPins extends AXI4Spectrometer[FixedPoint] {
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


class SpectrometerParams(fftSize: Int = 1024) {
  val params : SpectrometerParameters[FixedPoint] = SpectrometerParameters (
    winParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        numPoints = fftSize,
        dataWidth = 16,
        binPoint  = 10,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/blacman.txt",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
      ),
      winRAMAddress = AddressSet(0x60000000, 0xFFF),
      winCSRAddress = AddressSet(0x60001000, 0xFF)
    )),
    fftParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = fftSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(fftSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(fftSize))(true),
        minSRAMdepth = fftSize, // memories larger than 64 should be mapped on block ram
        binPoint = 10
      ),
      fftAddress = AddressSet(0x60001100, 0xFF)
    )),
    magParams = Some(MagParamsAndAddresses(
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
      magAddress = AddressSet(0x60001200, 0xFF),
    )),
    accParams = Some(AccParamsAndAddresses(
      accParams = AccParams(
        proto    = FixedPoint(16.W, 10.BP),
        protoAcc = FixedPoint(32.W, 10.BP),
      ),
      accAddress   = AddressSet(0x60001300, 0xFF),
      accQueueBase = 0x60002000
    )),
    cfarParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 10.BP),
        protoThreshold = FixedPoint(16.W, 10.BP),
        protoScaler = FixedPoint(16.W, 10.BP),
        leadLaggWindowSize = 64,
        guardWindowSize = 8,
        logOrLinReg = false,
        fftSize = fftSize,
        sendCut = true,
        minSubWindowSize = Some(4),
        includeCASH = true, //true
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,                  // number of add pipeline registers
        numMulPipes = 1                   // number of mull pipeline registers
      ),
      cfarAddress   = AddressSet(0x60001400, 0xFF),
    ))
  )
}

object SpectrometerApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpectrometerParams).params
  val lazyDut = LazyModule(new AXI4Spectrometer(params, 4) with AXI4SpectrometerPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/Spectrometer"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}