SPECTROMETER_PATH = ./

SIZE?=512
DEPTH?=128
ENABLE_PLOT?=false

export FFT_SIZE=$(SIZE)
export FFT_DEPTH=$(DEPTH)

# Forgot --recurse-submodules while cloning? I got you
init:
	git submodule update --init --recursive

verilog_spectrometer:
	cd $(SPECTROMETER_PATH); sbt "runMain spectrometer_v2.SpectrometerApp $(FFT_SIZE) $(FFT_DEPTH)"; cd -;


test_spectrometer:
	cd $(SPECTROMETER_PATH); SBT_OPTS="-DfftSize=$(FFT_SIZE) -DminSRAMDepth=$(FFT_SIZE) -DenablePlot=$(ENABLE_PLOT)" sbt "testOnly spectrometer_v2.SpectrometerTestSpec"; cd -;

check-env:
ifndef PDK_ROOT
	$(error PDK_ROOT is undefined, please export it before running make)
endif
ifndef RISCV32
	$(error RISCV32 is undefined, please export it before running make)
endif