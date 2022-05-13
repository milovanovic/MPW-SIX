HYPERSPACE_PATH = ./

SIZE?=512
DEPTH?=128
ENABLE_PLOT?=true

export FFT_SIZE=$(SIZE)
export FFT_DEPTH=$(DEPTH)

# Forgot --recurse-submodules while cloning? I got you
init:
	git submodule update --init --recursive

verilog_hyperspace:
	cd $(HYPERSPACE_PATH); sbt "runMain hyperspace.HyperSpaceApp $(FFT_SIZE) $(FFT_DEPTH)"; cd -;


test_hyperspace:
	cd $(HYPERSPACE_PATH); SBT_OPTS="-DfftSize=$(FFT_SIZE) -DminSRAMDepth=$(FFT_SIZE) -DenablePlot=$(ENABLE_PLOT)" sbt "testOnly hyperspace.HyperSpaceTestSpec"; cd -;

lint:
	verilator --lint-only -Wno-STMTDLY  -DSIM -DMPRJ_IO_PADS=38 \
	-I./verilog/rtl/HyperSpace \
	-I./verilog/rtl/wb2axip/rtl \
	-I./verilog/rtl/SRAM \
	./verilog/rtl/Top/user_project_wrapper.v

check-env:
ifndef PDK_ROOT
	$(error PDK_ROOT is undefined, please export it before running make)
endif
ifndef RISCV32
	$(error RISCV32 is undefined, please export it before running make)
endif