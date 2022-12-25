tbi68k.s37: tbi68k.rel
	./aslink -nsu $<

tbi68k.rel: tbi68k.asm
	./as68k -lo+$@ $<

clean:
	rm -f *.lst *.hlr *.rel *.rst *.s37
