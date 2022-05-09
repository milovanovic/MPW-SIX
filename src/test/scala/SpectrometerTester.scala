// SPDX-License-Identifier: Apache-2.0

package spectrometer_v2

import chisel3._
import chisel3.util.log2Ceil
import chisel3.iotesters.PeekPokeTester

import dsptools.numbers._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

import breeze.math.Complex
import breeze.signal.{fourierTr}
import breeze.linalg._

import scala.math._
import scala.util.Random
import java.io._

import cfar._

/* AXI4Spectrometer */
class SpectrometerTester[T <: Data : Real: BinaryRepresentation]
(
  dut: AXI4Spectrometer[T] with AXI4SpectrometerPins,
  params: SpectrometerParameters[T],
  fftSize : Int,
  runTime : Boolean = false,
  enablePlot: Boolean = false,
  silentFail: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {

  val mod = dut.module
  def memAXI: AXI4Bundle = dut.ioMem.get
  val master = bindMaster(dut.in)

  val noise = (0 until fftSize).map(i => Complex(math.sqrt(Random.nextDouble + Random.nextDouble) * math.pow(2, 10),math.sqrt(Random.nextDouble + Random.nextDouble) * math.pow(2, 10)))
  val s1    = (0 until fftSize).map(i => Complex(0.4 * math.cos(2 * math.Pi * 1/8 * i) * math.pow(2, 13), 0.4 * math.sin(2 * math.Pi * 1/8 * i) * math.pow(2, 13)))
  val s2    = (0 until fftSize).map(i => Complex(0.2 * math.cos(2 * math.Pi * 1/4 * i) * math.pow(2, 13), 0.2 * math.sin(2 * math.Pi * 1/4 * i) * math.pow(2, 13)))
  val s3    = (0 until fftSize).map(i => Complex(0.1 * math.cos(2 * math.Pi * 1/2 * i) * math.pow(2, 13), 0.1 * math.sin(2 * math.Pi * 1/2 * i) * math.pow(2, 13)))

  // can be simplified
  var sum   = noise.zip(s1).map { case (a, b) => a + b}.zip(s2).map{ case (c, d) => c + d }.zip(s3).map{ case (e, f)  => e + f }
  val fft = fourierTr(DenseVector(sum.toArray)).toScalaVector
  val fftScala = fourierTr(DenseVector(sum.toArray)).toScalaVector.map(c => Complex(c.real/fftSize, c.imag/fftSize))
  val fftMagScala = fftScala.map(c => c.abs.toInt)
  var testSignal: Seq[Double] = fft.map(c => math.sqrt(pow(c.real,2) + pow(c.imag,2)))
  
  // split 32 bit data to 4 bytes and send real sinusoid
  var dataByte = Seq[Int]()
  for (i <- sum) {
    // imag part
    dataByte = dataByte :+ ((i.imag.toInt)       & 0xFF)
    dataByte = dataByte :+ ((i.imag.toInt >>> 8) & 0xFF)
    // real part
    dataByte = dataByte :+ ((i.real.toInt)       & 0xFF)
    dataByte = dataByte :+ ((i.real.toInt >>> 8) & 0xFF)
  }

  // magAddress
  if(params.magParams != None) {
    memWriteWord(params.magParams.get.magAddress.base, 0x2) // set jpl magnitude
  }

  step(1)
  // add master transactions
  master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i), last = (i == dataByte.size-1))))
  // master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i), last = (i == dataByte.size-1))))
  // master.addTransactions((0 until dataByte.size).map(i => AXI4StreamTransaction(data = dataByte(i), last = (i == dataByte.size-1))))

  if(params.cfarParams != None) {
    val beatBytes = 4
    val cfarModeString = "CASH"
    val thresholdScaler = 2.8
    val binPointThresholdScaler = 10
    val thresholdScalerReg = (thresholdScaler * scala.math.pow(2,binPointThresholdScaler)).toInt
    val peakGrouping = 0

    val cfarMode = cfarModeString match {
      case "Cell Averaging" => 0
      case "Greatest Of" => 1
      case "Smallest Of" => 2
      case "CASH" => 3
      case _ => 0
    }

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
          else if (cfarModeString != "CASH") {
            subWindowSize = lWinSize
            refCells = lWinSize
          }
          else {
            subWindowSize = params.cfarParams.get.cfarParams.minSubWindowSize.get
            refCells = lWinSize
          }
          memWriteWord(params.cfarParams.get.cfarAddress.base + 0x7*beatBytes, subWindowSize) // subCells
        }
        else {
           refCells = lWinSize
        }
        println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize and subWindowSize = $subWindowSize")

        val considerEdges = if (params.cfarParams.get.cfarParams.includeCASH == true) false else true
        val (expThr, expPeaks) = if (params.cfarParams.get.cfarParams.includeCASH && cfarModeString == "CASH")
                                    CFARUtils.cfarCASH(testSignal, referenceCells = refCells, subCells = subWindowSize, scalingFactor = thresholdScaler, plotEn = enablePlot)
                                 else
                                    CFARUtils.cfarCA(testSignal, cfarMode = cfarModeString, referenceCells = lWinSize, guardCells = guardSize, considerEdges = considerEdges, scalingFactor = thresholdScaler, plotEn = enablePlot)

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

      }
    }

    var outSeq = Seq[Int]()
    var peekedVal: BigInt = 0
    var tempCounter = 0
    
    while (outSeq.length < fftSize * 3 && tempCounter < 10000) {
      if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
        peekedVal = peek(dut.out.bits.data)
        outSeq = outSeq :+ peekedVal.toInt
      }
      step(1)
      tempCounter = tempCounter + 1
    }

    var outCUT  = Seq[Int]()
    var outBIN  = Seq[Int]()
    var outPEAK = Seq[Int]()
    var outTreshold = Seq[Int]()
    var tempString: String = ""
    
    for (i <- 0 until outSeq.length by 3) {
      tempString = SpectrometerTesterUtils.asNdigitBinary(outSeq(i + 2), 16) ++ SpectrometerTesterUtils.asNdigitBinary(outSeq(i + 1), 16) ++ SpectrometerTesterUtils.asNdigitBinary(outSeq(i), 16)
      outTreshold = outTreshold :+ (java.lang.Integer.parseInt(tempString.substring(6,22) ,2).toShort).toInt
      outCUT      = outCUT      :+ (java.lang.Integer.parseInt(tempString.substring(22,38) ,2).toShort).toInt
      outBIN      = outBIN      :+ (java.lang.Integer.parseInt(tempString.substring(38,47) ,2).toShort).toInt
      outPEAK     = outPEAK     :+ (java.lang.Integer.parseInt(tempString.substring(47,48) ,2).toShort).toInt
    }

    println(s"Output sequence length : ${outTreshold.length}")

    // Write output data to text file
    val file = new File("./test_run_dir/AXI4Spectrometer/outCUT.txt")
    val w = new BufferedWriter(new FileWriter(file))
    for (i <- 0 until outCUT.length ) {
      w.write(f"${outCUT(i)}%04x" + "\n")
    }
    w.close

    val file1 = new File("./test_run_dir/AXI4Spectrometer/fftMagScala.txt")
    val w1 = new BufferedWriter(new FileWriter(file1))
    for (i <- 0 until fftMagScala.length ) {
      w1.write(f"${fftMagScala(i)}%04x" + "\n")
    }
    w1.close
    
    // check tolerance
    if (params.fftParams.get.fftParams.useBitReverse) {
      SpectrometerTesterUtils.checkDataError(outCUT, fftMagScala, 3)
    }
    else {
      val bRImag = SpectrometerTesterUtils.bitrevorder_data(outCUT)
      SpectrometerTesterUtils.checkDataError(bRImag, fftMagScala, 3)
    }

    

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
  
  if (enablePlot) {
    SpectrometerTesterUtils.plot_data(inputData = sum.map(c => c.real.toInt), plotName = "Input real data", fileName = "AXI4Spectrometer/in_real.pdf")
    SpectrometerTesterUtils.plot_data(inputData = sum.map(c => c.imag.toInt), plotName = "Input imag data", fileName = "AXI4Spectrometer/in_imag.pdf")
  }

  
  step(20000)
  stepToCompletion(silentFail = silentFail)
}