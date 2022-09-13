#!/bin/sh
echo "Creating base.dsk ..."
iDSK base.dsk -n
iDSK base.dsk -i ../AMDDOS.SCR -t 2
iDSK base.dsk -i ../ATARI.SCR -t 2
iDSK base.dsk -i ../DOUBLE.SCR -t 2
iDSK base.dsk -i ../GRAFDEMO.SCR -t 2
iDSK base.dsk -i ../GRAFIK.SCR -t 2
iDSK base.dsk -i ../INSTALL.SCR -t 2
iDSK base.dsk -i ../MATHE.SCR -t 2
iDSK base.dsk -i ../TERMINAL.SCR -t 2
iDSK base.dsk -i ../TURTDEMO.SCR -t 2
iDSK base.dsk -i ../TURTLE.SCR -t 2
iDSK base.dsk -i ../VDOS62KX.SCR -t 2
iDSK base.dsk -i ../VOLKS4TH.COM -t 2
echo "base.dsk created!"

echo "Creating kernel.dsk ..."
iDSK kernel.dsk -n
iDSK kernel.dsk -i ../ASS8080.SCR    -t 2
iDSK kernel.dsk -i ../ASSTRAN.SCR	   -t 2
iDSK kernel.dsk -i ../DISASS.SCR     -t 2
iDSK kernel.dsk -i ../FILEINT.SCR    -t 2
iDSK kernel.dsk -i ../HASHCASH.SCR   -t 2
iDSK kernel.dsk -i ../KERNEL.COM     -t 2
iDSK kernel.dsk -i ../PORT8080.SCR   -t 2
iDSK kernel.dsk -i ../PORTZ80.SCR    -t 2
iDSK kernel.dsk -i ../PRIMED.SCR     -t 2
iDSK kernel.dsk -i ../SIMPFILE.SCR   -t 2
iDSK kernel.dsk -i ../TIMES.SCR      -t 2
echo "kernel.dsk created!"

echo "Creating tools.dsk ..."
iDSK tools.dsk -n
iDSK tools.dsk -i ../ASS8080.SCR  -t 2
iDSK tools.dsk -i ../COPY.SCR     -t 2
iDSK tools.dsk -i ../EDITOR.SCR   -t 2
iDSK tools.dsk -i ../PRINTER.SCR  -t 2
iDSK tools.dsk -i ../RELOCATE.SCR -t 2
iDSK tools.dsk -i ../SAVESYS.SCR  -t 2
iDSK tools.dsk -i ../SEE.SCR      -t 2
iDSK tools.dsk -i ../STARTUP.SCR  -t 2
iDSK tools.dsk -i ../TASKER.SCR   -t 2
iDSK tools.dsk -i ../TERMINAL.SCR -t 2
iDSK tools.dsk -i ../TOOLS.SCR    -t 2
iDSK tools.dsk -i ../XINOUT.SCR   -t 2
echo "tools.dsk created!"

echo "Creating source.dsk ..."
iDSK source.dsk -n
iDSK source.dsk -i ../SOURCE.SCR  -t 2
echo "source.dsk created!"

