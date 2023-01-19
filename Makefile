tbi68k.s37: tbi68k.rel
	./aslink -nsu -a text=0 $<

tbi68k.rel: tbi68k.asm
	./as68k -lo+$@ $<

clean:
	rm -f *.lst *.hlr *.rel *.rst *.s37
