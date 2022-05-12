# WB2AXIP: Bus interconnects, bridges, and other components

The bus components and bridges within this repository are unique in that they
are all designed for 100% throughput with no throughput overhead.  They are also
unique in that the vast majority of the cores within have all been formally
verified.

Where the protocol allows it, such as with [AXI4](https://zipcpu.com/blog/2019/05/29/demoaxi.html),
[AXI-lite](https://zipcpu.com/formal/2018/12/28/axilite.html), and [Wishbone B4
pipelined](https://zipcpu.com/zipcpu/2017/11/07/wb-formal.html), multiple transactions
may be in flight at a time so that protocol handling doesn't stall the bus.

# Licensing

This repository is licensed under the [Apache 2
license](https://www.apache.org/licenses/LICENSE-2.0.html).

# Thanks

I'd like to thank @wallento for his initial work on a
[Wishbone to AXI converter](https://github.com/wallento/wb2axi), and his
encouragement to improve upon it.  While this isn't a fork of his work, the
initial [pipelined wishbone to AXI bridge](rtl/wbm2axisp.v) which formed
the core seed for this project took its initial motivation from his work.

Many of the rest of these projects have been motivated by the desire to learn
and develop my formal verification skills.  For that, I would thank the staff
of Symbiotic EDA for their tools and their encouragement.
