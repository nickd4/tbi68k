tbi68k.s37: tbi68k.rel
	./aslink -p0 -a text=0x900 -s $<

tbi68k.rel: tbi68k.asm
	./as68k $<

clean:
	rm -f *.rel *.s37
