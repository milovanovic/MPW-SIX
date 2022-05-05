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

  // Scala fft
  val fftScala = fourierTr(DenseVector(inData.toArray)).toScalaVector.map(c => Complex(c.real/fftSize, c.imag/fftSize))
  val fftMagScala = fftScala.map(c => c.abs)
  
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

    var lWinSizes: Seq[Int] = Seq()
    if (runTime)
      lWinSizes = CFARUtils.pow2Divisors(params.cfarParams.get.cfarParams.leadLaggWindowSize).filter(_ > 2).toSeq
    else
      lWinSizes = Seq(params.cfarParams.get.cfarParams.leadLaggWindowSize)
    val startGwin: Int = if (runTime) 2 else params.cfarParams.get.cfarParams.guardWindowSize
    var subWindowSize: Int = params.cfarParams.get.cfarParams.minSubWindowSize.getOrElse(params.cfarParams.get.cfarParams.leadLaggWindowSize)
    var refCells: Int = params.cfarParams.get.cfarParams.leadLaggWindowSize
    
    for (lWinSize <- lWinSizes) {
      for (guardSize <- startGwin to params.cfarParams.get.cfarParams.guardWindowSize) {
        // form here output data
        if (params.cfarParams.get.cfarParams.includeCASH) {
          if (params.cfarParams.get.cfarParams.minSubWindowSize.get > lWinSize) {
            refCells =  params.cfarParams.get.cfarParams.minSubWindowSize.get
          }
          else if (cfarMode != "CASH") {
            subWindowSize = lWinSize
            refCells = lWinSize
          }
          else {
            subWindowSize = params.cfarParams.get.cfarParams.minSubWindowSize.get
            refCells = lWinSize
          }
          memWriteWord(params.cfarParams.get.cfarAddress.base + 0x7*beatBytes, lsubWindowSize) // subCells
        }
        else {
           refCells = lWinSize
        }
        println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize and subWindowSize = $subWindowSize")

        val considerEdges = if (params.cfarParams.get.cfarParams.includeCASH == true) false else true
        val (expThr, expPeaks) = if (params.cfarParams.get.cfarParams.includeCASH && cfarMode == "CASH")
                                    CFARUtils.cfarCASH(fftMagScala, referenceCells = refCells, subCells = subWindowSize, scalingFactor = thresholdScaler, plotEn = thrPlot)
                                 else
                                    CFARUtils.cfarCA(fftMagScala, cfarMode = cfarMode, referenceCells = lWinSize, guardCells = guardSize, considerEdges = considerEdges, scalingFactor = thresholdScaler, plotEn = thrPlot)

        if (cfarMode == 3) {
          memWriteWord(params.cfarParams.get.cfarAddress.base + 0x5*beatBytes, 0) // guardCells
        }
        else {
          memWriteWord(params.cfarParams.get.cfarAddress.base + 0x5*beatBytes, guardSize) // guardCells
        }
        memWriteWord(params.cfarParams.get.cfarAddress.base + 0x4*beatBytes, refCells) // windowCells

        if (params.cfarParams.get.cfarParams.CFARAlgorithm != GOSCFARType) {
          memWriteWord(params.cfarParams.get.cfarAddress.base + 0x6*beatBytes, log2Ceil(lWinSize)) // divSum
        }

        step(2) // be sure that control registers are first initilized and then set ready and valid signals
        poke(dut.out.ready, 1)

        for (i <- 0 until fftMagScala.size) {
          poke(dut.in.valid, 0)
          val delay = 3//Random.nextInt(5)
          //step(delay)
          for (i <- 0 until delay) {
            if (peek(dut.out.valid) == true) {
              params.cfarParams.get.cfarParams.protoIn match {
                case dspR: DspReal => {
                  realTolDecPts.withValue(tol) { expect(dut.out.bits.cut.get,  fftMagScala(cntValidOut).toDouble) }
                  realTolDecPts.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
                }
                case _ =>  {
                  fixTolLSBs.withValue(tol) { expect(dut.out.bits.cut.get, fftMagScala(cntValidOut)) }
                  fixTolLSBs.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
                }
              }
              //fftBin
              if (expPeaks.contains(peek(dut.fftBin))) {
                expect(dut.out.bits.peak, 1)
              }
              cntValidOut += 1
              threshold += peek(dut.out.bits.threshold)
            }
            step(1)
          }
          poke(dut.in.valid, 1)
          poke(dut.in.bits.data, fftMagScala(i))
          if (i == (fftMagScala.size - 1))
            poke(dut.in.bits.last, 1)
            if (peek(dut.out.valid) == true) {
              params.cfarParams.get.cfarParams.protoIn match {
                case dspR: DspReal => {
                  realTolDecPts.withValue(tol) { expect(dut.out.bits.cut.get,  fftMagScala(cntValidOut).toDouble) }
                  realTolDecPts.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
                }
                case _ =>  {
                  fixTolLSBs.withValue(tol) { expect(dut.out.bits.cut.get, fftMagScala(cntValidOut)) }
                  fixTolLSBs.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
                }
              }
              //fftBin
              if (expPeaks.contains(peek(dut.fftBin))) {
                expect(dut.out.bits.peak, 1)
              }
              cntValidOut += 1
              threshold += peek(dut.out.bits.threshold)
            }
          step(1)
        }
        poke(dut.in.bits.last, 0)
        poke(dut.in.valid, 0)
        poke(dut.out.ready, 0)
        step(10)
        poke(dut.out.ready, 1)
       // println("Value of the counter is:")
       // println(cntValidOut.toString)
        while (cntValidOut < in.size) {
          if (peek(dut.out.valid) && peek(dut.out.ready)) {
            params.cfarParams.get.cfarParams.protoIn match {
              case dspR: DspReal => {
                realTolDecPts.withValue(tol) { expect(dut.out.bits.cut.get, fftMagScala(cntValidOut)) }
                realTolDecPts.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
              }
              case _ =>  {
                fixTolLSBs.withValue(tol) { expect(dut.out.bits.cut.get, fftMagScala(cntValidOut)) }
                fixTolLSBs.withValue(tol) { expect(dut.out.bits.threshold, expThr(cntValidOut)) }
              }
            }
            if (expPeaks.contains(peek(dut.fftBin))) {
              expect(dut.out.bits.peak, 1)
            }
            threshold += peek(dut.out.bits.threshold)

            if (cntValidOut == fftMagScala.size - 1)
              expect(dut.lastOut, 1)
            cntValidOut += 1
          }
          step(1)
      }

      cntValidOut = 0
      step(params.cfarParams.get.cfarParams.leadLaggWindowSize * 2)
      }
    }
  }

  poke(dut.out.ready, true)

  step(1)
   // add master transactions
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i))))


  var outSeq = Seq[BigInt]()
  var peekedVal: BigInt = 0
  
  // check only one fft window 
  while (outSeq.length < fftSize * 3) {
    if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
      peekedVal = peek(dut.out.bits.data)
      outSeq = outSeq :+ peekedVal
    }
    step(1)
  }


    
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
  stepToCompletion(silentFail = silentFail)
}