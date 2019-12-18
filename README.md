# Simple VGA Controller

## Features

* Work in progress! Uploaded to github in the hope that someone else finds this interesting or useful
* Written in VHDL
* Runs at 640x480, 60Hz; a 25.175MHz pixel clock is required
* Tested with a 68000 as the host controller, but may work with outher MPUs/MCUs
* The FPGA I'm using in an ancient EPF10K10 in PLCC84, which seems to be a pretty nice part for a 20 year old 5V throughole mountable FPGA
   * But any part, especially older Alteras, should work
   * Because I'm pin-restricted only an 8 bit databus is presented
* Video memory is 3 x 32KB 12nS SRAM, though using other SRAMs should be a trivial change
* Connection to DB15 is: horizontal sync, vertical sync and 3 bits of Red, Green, and Blue
* Text mode:
   * 80x60
   * Uses 3x256 bytes of FPGA RAM bits to hold the font
   * The included MIF files contain the font used by the Amstrad CPC, but any 8x8 font will work
   * Automatic hardware cursor
   * Two bytes per character: second byte is attributes:
      * 0: Red foreground
      * 1: Green foreground
      * 2: Blue foreground
      * 3: Brightness foreground
      * 4-7: Same for background
* Bitmap mode:
   * Am experimenting with a simple 1bpp bitmap mode

## Registers

* 0: DATA - read and write to the READWRITE_ADDR_HI/LO
* 1: DEFAULT_ATTRIBUTE - the attribute to use when the one read out of SRAM is 0
* 2: MODE1 - bit 0: if this is 1 then the 1bpp bitmap is enabled, otherwise text mode is used
* 3: MODE1 - unused
* 4: READWRITE_ADDR_HI: the high portion of the read/write pointer
* 5: READWRITE_ADDR_LO: the low portion
* 6: OFFSET_ADDR_HI: the high portion of the frame offset (used for hardware scrolling)
* 7: OFFSET_ADDR_LO: the low portionOnly the DATA register is readable.

The read action will return a nonsense value on the first read, as reads arebuffered. The READWRITE_ADDR is of course auto-incrementing.

OFFSET_ADDR_HI/LO is used for scrolling. Eg. to move the image down one line, add 80 (decimal) to the OFFSET_ADDR. The offset is used for the display drawing only. The offset works in both text and bitmap modes.

## Notes

DTACK and interrupts are not yet done. With a 16MHz MPU DTACK does not seemto be required.

Total resource usage is around 500 Logic Elements in a Flex 10K FPGA, and 3 lots of 2048 RAM bits, which is the total capacity of the EPF10K10.

The testbench is lacking. It can generate graphs of the vramcontroller entity though, which is the core of the display generator.
