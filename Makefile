SPECTROMETER_PATH = ./

SIZE?=512
export FFT_SIZE=$(SIZE)

	
verilog_spectrometer:
	cd $(SPECTROMETER_PATH); sbt "runMain spectrometer_v2.SpectrometerApp $(FFT_SIZE)"; cd -;

	
check-env:
ifndef PDK_ROOT
	$(error PDK_ROOT is undefined, please export it before running make)
endif
ifndef RISCV32
	$(error RISCV32 is undefined, please export it before running make)
endif