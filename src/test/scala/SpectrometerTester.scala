// SPDX-License-Identifier: Apache-2.0

package spectrometer_v2

import chisel3._
import chisel3.iotesters.PeekPokeTester

import dsptools.numbers._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

import breeze.math.Complex
import breeze.signal.{fourierTr}
import breeze.linalg._

import java.io._


/* AXI4Spectrometer */
class SpectrometerTester[T <: Data : Real: BinaryRepresentation]
(
  dut: AXI4Spectrometer[T] with AXI4SpectrometerPins,
  params: SpectrometerParameters[T],
  fftSize : Int,
  enablePlot: Boolean = false,
  silentFail: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {

  val mod = dut.module
  def memAXI: AXI4Bundle = dut.ioMem.get
  val master = bindMaster(dut.in)
  
  val binWithPeak = 2
  val inData = SpectrometerTesterUtils.getTone(numSamples = fftSize, binWithPeak.toDouble/fftSize.toDouble)
  
  // split 32 bit data to 4 bytes and send real sinusoid
  var dataByte = Seq[Int]()
  for (i <- inData) {
    // imag part
    dataByte = dataByte :+ 0
    dataByte = dataByte :+ 0
    // real part
    dataByte = dataByte :+ ((i)        & 0xFF)
    dataByte = dataByte :+ ((i >>> 8)  & 0xFF)
  }

  // magAddress
  if(params.magParams != None) {
    memWriteWord(params.magParams.get.magAddress.base, 0x2) // set jpl magnitude
  }

  if(params.cfarParams != None) {
    val beatBytes = 4
    val cfarMode = 1
    val refWindowSize = 16
    val guardWindowSize = 4
    val subWindowSize = refWindowSize
    val thresholdScaler = 2.8
    val binPointThresholdScaler = 10
    val thresholdScalerReg = (thresholdScaler * scala.math.pow(2,binPointThresholdScaler)).toInt
    val divSum = (scala.math.log10(refWindowSize)/scala.math.log10(2.0)).toInt
    val peakGrouping = 0

    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x0*beatBytes, fftSize)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x1*beatBytes, thresholdScalerReg)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x2*beatBytes, peakGrouping)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x3*beatBytes, cfarMode)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x4*beatBytes, refWindowSize)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x5*beatBytes, guardWindowSize)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x6*beatBytes, divSum)
    memWriteWord(params.cfarParams.get.cfarAddress.base + 0x7*beatBytes, subWindowSize)
  }

  poke(dut.out.ready, true)

  step(1)
   // add master transactions
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))


  // var outSeq = Seq[Int]()
  // var peekedVal: BigInt = 0
  
  // // check only one fft window 
  // while (outSeq.length < fftSize * 6) {
  //   if (peek(dut.outStream.valid) == 1 && peek(dut.outStream.ready) == 1) {
  //     peekedVal = peek(dut.outStream.bits.data)
  //     outSeq = outSeq :+ peekedVal.toInt
  //   }
  //   step(1)
  // }

  // var realSeq = Seq[Int]()
  // var imagSeq = Seq[Int]()
  // var tmpReal: Short = 0
  // var tmpImag: Short = 0
  
  // for (i <- 0 until outSeq.length by 6) {
  //   tmpReal = java.lang.Integer.parseInt(SpectrometerTesterUtils.asNdigitBinary(outSeq(i + 3), 8) ++ SpectrometerTesterUtils.asNdigitBinary(outSeq(i + 2), 8), 2).toShort
  //   tmpImag = java.lang.Long.parseLong(SpectrometerTesterUtils.asNdigitBinary(outSeq(i + 1), 8)   ++ SpectrometerTesterUtils.asNdigitBinary(outSeq(i), 8), 2).toShort
  //   realSeq = realSeq :+ tmpReal.toInt
  //   imagSeq = imagSeq :+ tmpImag.toInt
  // }
  
  // // Scala fft
  // val fftScala = fourierTr(DenseVector(inData.toArray)).toScalaVector.map(c => Complex(c.real/fftSize, c.imag/fftSize))
  // val fftMagScala = fftScala.map(c => c.abs.toInt)
  
  // imagSeq.foreach( c => println(c.toString))
  // fftMagScala.foreach( c => println(c.toString))
  
  // // check tolerance
  // if (params.fftParams.useBitReverse) {
  //   SpectrometerTesterUtils.checkDataError(imagSeq, fftMagScala, 3)
  // }
  // else {
  //   val bRImag = SpectrometerTesterUtils.bitrevorder_data(imagSeq)
  //   SpectrometerTesterUtils.checkDataError(bRImag, fftMagScala, 3)
  // }
  
  // if (enablePlot) {
  //   SpectrometerTesterUtils.plot_data(inputData = inData, plotName = "Input data", fileName = "SpectrometerTest/pin_fft_mag_pout/plot_in.pdf")
  //   SpectrometerTesterUtils.plot_data(inputData = imagSeq.map(c => c.toInt), plotName = "PIN -> FFT -> MAG -> POUT", fileName = "SpectrometerTest/pin_fft_mag_pout/plot_mag.pdf")
  // }
  step(20000)
  stepToCompletion(silentFail = silentFail)
}