tbi68k.s37: tbi68k.rel
	./aslink -p0 -s -u $<

tbi68k.rel: tbi68k.asm
	./as68k -l -o $@ $<

clean:
	rm -f *.lst *.hlr *.rel *.rst *.s37
