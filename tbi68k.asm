;*****************************************************************
;								 *
;		Tiny BASIC for the Motorola MC68000		 *
;								 *
; Derived from Palo Alto Tiny BASIC as published in the May 1976 *
; issue of Dr. Dobb's Journal.  Adapted to the 68000 by:         *
;	Gordon Brandly						 *
;	12147 - 51 Street					 *
;	Edmonton AB  T5W 3G8					 *
;	Canada							 *
;	(updated mailing address for 1996)			 *
;								 *
; This version is for MEX68KECB Educational Computer Board I/O.  *
;								 *
;*****************************************************************
;    Copyright (C) 1984 by Gordon Brandly. This program may be	 *
;    freely distributed for personal use only. All commercial	 *
;		       rights are reserved.			 *
;*****************************************************************

; Vers. 1.0  1984/7/17	- Original version by Gordon Brandly
;	1.1  1984/12/9	- Addition of '$' print term by Marvin Lipford
;	1.2  1985/4/9	- Bug fix in multiply routine by Rick Murray

	;opt	frs,brs 	; forward ref.'s & branches default to short

cr	=	0x0d		; ASCII equates
lf	=	0x0a
tab	=	0x09
ctrlc	=	0x03
ctrlh	=	0x08
ctrls	=	0x13
ctrlx	=	0x18

buflen	=	80		; length of keyboard input buffer

	.area	text (dpage)	; org 0x900, first free address using Tutor

;
; Standard jump table. You can change these addresses if you are
; customizing this interpreter for a different environment.
;
start:	bra.l	cstart		; Cold Start entry point
gowarm:	bra.l	wstart		; Warm Start entry point
goout:	bra.l	outc		; Jump to character-out routine
goin:	bra.l	inc		; Jump to character-in routine
goauxo:	bra.l	auxout		; Jump to auxiliary-out routine
goauxi:	bra.l	auxin		; Jump to auxiliary-in routine
gobye:	bra.l	byebye		; Jump to monitor, DOS, etc.
;
; Modifiable system constants:
;
txtbgn:	.long	txt		; beginning of program memory
endmem:	.long	0x8000		; end of available memory
;
; The main interpreter starts here:
;
cstart:	move.l	endmem,sp	; initialize stack pointer
	lea	initmsg,a6	; tell who we are
	bsr.l	prmesg
	move.l	txtbgn,txtunf	; init. end-of-program pointer
	move.l	endmem,d0	; get address of end of memory
	sub.l	#2048,d0	; reserve 2K for the stack
	move.l	d0,stklmt
	sub.l	#108,d0 	; reserve variable area (27 long words)
	move.l	d0,varbgn
wstart:	clr.l	d0		; initialize internal variables
	move.l	d0,lopvar
	move.l	d0,stkgos
	move.l	d0,currnt	; current line number pointer = 0
	move.l	endmem,sp	; init S.P. again, just in case
	lea	okmsg,a6	; display "OK"
	bsr.l	prmesg
st3:	move.b	#'>,d0		; Prompt with a '>' and
	bsr.l	getln		; read a line.
	bsr.l	toupbuf 	; convert to upper case
	move.l	a0,a4		; save pointer to end of line
	lea	buffer,a0	; point to the beginning of line
	bsr.l	tstnum		; is there a number there?
	bsr.l	ignblk		; skip trailing blanks
	tst	d1		; does line no. exist? (or nonzero?)
	beq.l	direct		; if not, it's a direct statement
	cmp.l	#0xffff,d1	; see if line no. is <= 16 bits
	bcc.l	qhow		; if not, we've overflowed
	move.b	d1,-(a0)	; store the binary line no.
	ror	#8,d1		; (Kludge to store a word on a
	move.b	d1,-(a0)	; possible byte boundary)
	rol	#8,d1
	bsr.l	fndln		; find this line in save area
	move.l	a1,a5		; save possible line pointer
	bne	st4		; if not found, insert
	bsr.l	fndnxt		; find the next line (into A1)
	move.l	a5,a2		; pointer to line to be deleted
	move.l	txtunf,a3	; points to top of save area
	bsr.l	mvup		; move up to delete
	move.l	a2,txtunf	; update the end pointer
st4:	move.l	a4,d0		; calculate the length of new line
	sub.l	a0,d0
	cmp.l	#3,d0		; is it just a line no. & CR?
	beq	st3		; if so, it was just a delete
	move.l	txtunf,a3	; compute new end
	move.l	a3,a6
	add.l	d0,a3
	move.l	varbgn,d0	; see if there's enough room
	cmp.l	a3,d0
	bls.l	qsorry		; if not, say so
	move.l	a3,txtunf	; if so, store new end position
	move.l	a6,a1		; points to old unfilled area
	move.l	a5,a2		; points to beginning of move area
	bsr.l	mvdown		; move things out of the way
	move.l	a0,a1		; set up to do the insertion
	move.l	a5,a2
	move.l	a4,a3
	bsr.l	mvup		; do it
	bra	st3		; go back and get another line

;
;******************************************************************
;
; *** Tables *** DIRECT *** EXEC ***
;
; This section of the code tests a string against a table. When
; a match is found, control is transferred to the section of
; code according to the table.
;
; At 'EXEC', A0 should point to the string, A1 should point to
; the character table, and A2 should point to the execution
; table. At 'DIRECT', A0 should point to the string, A1 and
; A2 will be set up to point to TAB1 and TAB1.1, which are
; the tables of all direct and statement commands.
;
; A '.' in the string will terminate the test and the partial
; match will be considered as a match, e.g. 'P.', 'PR.','PRI.',
; 'PRIN.', or 'PRINT' will all match 'PRINT'.
;
; There are two tables: the character table and the execution
; table. The character table consists of any number of text items.
; Each item is a string of characters with the last character's
; high bit set to one. The execution table holds a 16-bit
; execution addresses that correspond to each entry in the
; character table.
;
; The end of the character table is a 0 byte which corresponds
; to the default routine in the execution table, which is
; executed if none of the other table items are matched.
;
; Character-matching tables:
tab1:	.byte	'L,'I,'S,('T+0x80)	; Direct commands
	.byte	'L,'O,'A,('D+0x80)
	.byte	'N,'E,('W+0x80)
	.byte	'R,'U,('N+0x80)
	.byte	'S,'A,'V,('E+0x80)
tab2:	.byte	'N,'E,'X,('T+0x80)	; Direct / statement
	.byte	'L,'E,('T+0x80)
	.byte	'I,('F+0x80)
	.byte	'G,'O,'T,('O+0x80)
	.byte	'G,'O,'S,'U,('B+0x80)
	.byte	'R,'E,'T,'U,'R,('N+0x80)
	.byte	'R,'E,('M+0x80)
	.byte	'F,'O,('R+0x80)
	.byte	'I,'N,'P,'U,('T+0x80)
	.byte	'P,'R,'I,'N,('T+0x80)
	.byte	'P,'O,'K,('E+0x80)
	.byte	'S,'T,'O,('P+0x80)
	.byte	'B,'Y,('E+0x80)
	.byte	'C,'A,'L,('L+0x80)
	.byte	0
tab4:	.byte	'P,'E,'E,('K+0x80)	; Functions
	.byte	'R,'N,('D+0x80)
	.byte	'A,'B,('S+0x80)
	.byte	'S,'I,'Z,('E+0x80)
	.byte	0
tab5:	.byte	'T,('O+0x80)		; "TO" in "FOR"
	.byte	0
tab6:	.byte	'S,'T,'E,('P+0x80)	; "STEP" in "FOR"
	.byte	0
tab8:	.byte	'>,('=+0x80)		; Relational operators
	.byte	'<,('>+0x80)
	.byte	('>+0x80)
	.byte	('=+0x80)
	.byte	'<,('=+0x80)
	.byte	('<+0x80)
	.byte	0
	.byte	0	; <- for aligning on a word boundary

; Execution address tables:
tab1.1:	.word	list			; Direct commands
	.word	load
	.word	new
	.word	run
	.word	save
tab2.1:	.word	next			; Direct / statement
	.word	let
	.word	if
	.word	goto
	.word	gosub
	.word	return
	.word	rem
	.word	for
	.word	input
	.word	print
	.word	poke
	.word	stop
	.word	gobye
	.word	call
	.word	deflt
tab4.1:	.word	peek			; Functions
	.word	rnd
	.word	abs
	.word	size
	.word	xp40
tab5.1:	.word	fr1			; "TO" in "FOR"
	.word	qwhat
tab6.1:	.word	fr2			; "STEP" in "FOR"
	.word	fr3
tab8.1:	.word	xp11	; >=		; Relational operators
	.word	xp12	; <>
	.word	xp13	; >
	.word	xp15	; =
	.word	xp14	; <=
	.word	xp16	; <
	.word	xp17
;
direct:	lea	tab1,a1
	lea	tab1.1,a2
exec:	bsr.l	ignblk		; ignore leading blanks
	move.l	a0,a3		; save the pointer
	clr.b	d2		; clear match flag
exlp:	move.b	(a0)+,d0	; get the program character
	move.b	(a1),d1 	; get the table character
	bne	exngo		; If end of table,
	move.l	a3,a0		; restore the text pointer and...
	bra	exgo		; execute the default.
exngo:	move.b	d0,d3		; Else check for period...
	and.b	d2,d3		; and a match.
	cmp.b	#'.,d3
	beq	exgo		; if so, execute
	and.b	#0x7f,d1 	; ignore the table's high bit
	cmp.b	d0,d1		; is there a match?
	beq	exmat
	addq.l	#2,a2		; if not, try the next entry
	move.l	a3,a0		; reset the program pointer
	clr.b	d2		; sorry, no match
ex1:	tst.b	(a1)+		; get to the end of the entry
	bpl	ex1
	bra	exlp		; back for more matching
exmat:	moveq	#-1,d2		; we've got a match so far
	tst.b	(a1)+		; end of table entry?
	bpl	exlp		; if not, go back for more
exgo:	lea	0,a3		; execute the appropriate routine
	move	(a2),a3
	jmp	(a3)
;
;******************************************************************
;
; What follows is the code to execute direct and statement
; commands. Control is transferred to these points via the command
; table lookup code of 'DIRECT' and 'EXEC' in the last section.
; After the command is executed, control is transferred to other
; sections as follows:
;
; For 'LIST', 'NEW', and 'STOP': go back to the warm start point.
; For 'RUN': go execute the first stored line if any; else go
; back to the warm start point.
; For 'GOTO' and 'GOSUB': go execute the target line.
; For 'RETURN' and 'NEXT'; go back to saved return line.
; For all others: if 'CURRNT' is 0, go to warm start; else go
; execute next command. (This is done in 'FINISH'.)
;
;******************************************************************
;
; *** NEW *** STOP *** RUN (& friends) *** GOTO ***
;
; 'NEW<CR>' sets TXTUNF to point to TXTBGN
;
; 'STOP<CR>' goes back to WSTART
;
; 'RUN<CR>' finds the first stored line, stores its address
; in CURRNT, and starts executing it. Note that only those
; commands in TAB2 are legal for a stored program.
;
; There are 3 more entries in 'RUN':
; 'RUNNXL' finds next line, stores it's address and executes it.
; 'RUNTSL' stores the address of this line and executes it.
; 'RUNSML' continues the execution on same line.
;
; 'GOTO expr<CR>' evaluates the expression, finds the target
; line, and jumps to 'RUNTSL' to do it.
;
new:	bsr.l	endchk
	move.l	txtbgn,txtunf	; set the end pointer

stop:	bsr.l	endchk
	bra	wstart

run:	bsr.l	endchk
	move.l	txtbgn,a0	; set pointer to beginning
	move.l	a0,currnt

runnxl:	tst.l	currnt		; executing a program?
	beq.l	wstart		; if not, we've finished a direct stat.
	clr.l	d1		; else find the next line number
	move.l	a0,a1
	bsr.l	fndlnp
	bcs	wstart		; if we've fallen off the end, stop

runtsl:	move.l	a1,currnt	; set CURRNT to point to the line no.
	move.l	a1,a0		; set the text pointer to
	addq.l	#2,a0		; the start of the line text

runsml:	bsr.l	chkio		; see if a control-C was pressed
	lea	tab2,a1 	; find command in TAB2
	lea	tab2.1,a2
	bra	exec		; and execute it

goto:	bsr.l	expr		; evaluate the following expression
	bsr.l	endchk		; must find end of line
	move.l	d0,d1
	bsr.l	fndln		; find the target line
	bne.l	qhow		; no such line no.
	bra	runtsl		; go do it

;
;******************************************************************
;
; *** LIST *** PRINT ***
;
; LIST has two forms:
; 'LIST<CR>' lists all saved lines
; 'LIST #<CR>' starts listing at the line #
; Control-S pauses the listing, control-C stops it.
;
; PRINT command is 'PRINT ....:' or 'PRINT ....<CR>'
; where '....' is a list of expressions, formats, back-arrows,
; and strings.	These items a separated by commas.
;
; A format is a pound sign followed by a number.  It controls
; the number of spaces the value of an expression is going to
; be printed in.  It stays effective for the rest of the print
; command unless changed by another format.  If no format is
; specified, 11 positions will be used.
;
; A string is quoted in a pair of single- or double-quotes.
;
; An underline (back-arrow) means generate a <CR> without a <LF>
;
; A <CR LF> is generated after the entire list has been printed
; or if the list is empty.  If the list ends with a semicolon,
; however, no <CR LF> is generated.
;

list:	bsr.l	tstnum		; see if there's a line no.
	bsr.l	endchk		; if not, we get a zero
	bsr.l	fndln		; find this or next line
ls1:	bcs	wstart		; warm start if we passed the end
	bsr.l	prtln		; print the line
	bsr.l	chkio		; check for listing halt request
	beq	ls3
	cmp.b	#ctrls,d0	; pause the listing?
	bne	ls3
ls2:	bsr.l	chkio		; if so, wait for another keypress
	beq	ls2
ls3:	bsr.l	fndlnp		; find the next line
	bra	ls1

print:	move	#11,d4		; D4 = number of print spaces
	bsr.l	tstc		; if null list and ":"
	.byte	':,pr2-.
	bsr.l	crlf		; give CR-LF and continue
	bra	runsml		; execution on the same line
pr2:	bsr.l	tstc		; if null list and <CR>
	.byte	cr,pr0-.
	bsr.l	crlf		; also give CR-LF and
	bra	runnxl		; execute the next line
pr0:	bsr.l	tstc		; else is it a format?
	.byte	'#,pr1-.
	bsr.l	expr		; yes, evaluate expression
	move	d0,d4		; and save it as print width
	bra	pr3		; look for more to print
pr1:	bsr.l	tstc		; is character expression? (MRL)
	.byte	'$,pr4-.
	bsr.l	expr		; yep. Evaluate expression (MRL)
	bsr	goout		; print low byte (MRL)
	bra	pr3		; look for more. (MRL)
pr4:	bsr.l	qtstg		; is it a string?
	bra.s	pr8		; if not, must be an expression
pr3:	bsr.l	tstc		; if ",", go find next
	.byte	',,pr6-.
	bsr.l	fin		; in the list.
	bra	pr0
pr6:	bsr.l	crlf		; list ends here
	bra	finish
pr8:	move	d4,-(sp)	; save the width value
	bsr.l	expr		; evaluate the expression
	move	(sp)+,d4	; restore the width
	move.l	d0,d1
	bsr.l	prtnum		; print its value
	bra	pr3		; more to print?

finish:	bsr.l	fin		; Check end of command
	bra.l	qwhat		; print "What?" if wrong

;
;******************************************************************
;
; *** GOSUB *** & RETURN ***
;
; 'GOSUB expr:' or 'GOSUB expr<CR>' is like the 'GOTO' command,
; except that the current text pointer, stack pointer, etc. are
; saved so that execution can be continued after the subroutine
; 'RETURN's.  In order that 'GOSUB' can be nested (and even
; recursive), the save area must be stacked.  The stack pointer
; is saved in 'STKGOS'.  The old 'STKGOS' is saved on the stack.
; If we are in the main routine, 'STKGOS' is zero (this was done
; in the initialization section of the interpreter), but we still
; save it as a flag for no further 'RETURN's.
;
; 'RETURN<CR>' undoes everything that 'GOSUB' did, and thus
; returns the execution to the command after the most recent
; 'GOSUB'.  If 'STKGOS' is zero, it indicates that we never had
; a 'GOSUB' and is thus an error.
;
gosub:	bsr.l	pusha		; save the current 'FOR' parameters
	bsr.l	expr		; get line number
	move.l	a0,-(sp)	; save text pointer
	move.l	d0,d1
	bsr.l	fndln		; find the target line
	bne.l	ahow		; if not there, say "How?"
	move.l	currnt,-(sp)	; found it, save old 'CURRNT'...
	move.l	stkgos,-(sp)	; and 'STKGOS'
	clr.l	lopvar		; load new values
	move.l	sp,stkgos
	bra	runtsl

return:	bsr.l	endchk		; there should be just a <CR>
	move.l	stkgos,d1	; get old stack pointer
	beq.l	qwhat		; if zero, it doesn't exist
	move.l	d1,sp		; else restore it
	move.l	(sp)+,stkgos	; and the old 'STKGOS'
	move.l	(sp)+,currnt	; and the old 'CURRNT'
	move.l	(sp)+,a0	; and the old text pointer
	bsr.l	popa		; and the old 'FOR' parameters
	bra	finish		; and we are back home

;
;******************************************************************
;
; *** FOR *** & NEXT ***
;
; 'FOR' has two forms:
; 'FOR var=exp1 TO exp2 STEP exp1' and 'FOR var=exp1 TO exp2'
; The second form means the same thing as the first form with a
; STEP of positive 1.  The interpreter will find the variable 'var'
; and set its value to the current value of 'exp1'.  It also
; evaluates 'exp2' and 'exp1' and saves all these together with
; the text pointer, etc. in the 'FOR' save area, which consisits of
; 'LOPVAR', 'LOPINC', 'LOPLMT', 'LOPLN', and 'LOPPT'.  If there is
; already something in the save area (indicated by a non-zero
; 'LOPVAR'), then the old save area is saved on the stack before
; the new values are stored.  The interpreter will then dig in the
; stack and find out if this same variable was used in another
; currently active 'FOR' loop.  If that is the case, then the old
; 'FOR' loop is deactivated. (i.e. purged from the stack)
;
; 'NEXT var' serves as the logical (not necessarily physical) end
; of the 'FOR' loop.  The control variable 'var' is checked with
; the 'LOPVAR'.  If they are not the same, the interpreter digs in
; the stack to find the right one and purges all those that didn't
; match.  Either way, it then adds the 'STEP' to that variable and
; checks the result with against the limit value.  If it is within
; the limit, control loops back to the command following the
; 'FOR'.  If it's outside the limit, the save area is purged and
; execution continues.
;
for:	bsr.l	pusha		; save the old 'FOR' save area
	bsr.l	setval		; set the control variable
	move.l	a6,lopvar	; save its address
	lea	tab5,a1 	; use 'EXEC' to test for 'TO'
	lea	tab5.1,a2
	bra	exec
fr1:	bsr.l	expr		; evaluate the limit
	move.l	d0,loplmt	; save that
	lea	tab6,a1 	; use 'EXEC' to look for the
	lea	tab6.1,a2	; word 'STEP'
	bra	exec
fr2:	bsr.l	expr		; found it, get the step value
	bra	fr4
fr3:	moveq	#1,d0		; not found, step defaults to 1
fr4:	move.l	d0,lopinc	; save that too
fr5:	move.l	currnt,lopln	; save address of current line number
	move.l	a0,loppt	; and text pointer
	move.l	sp,a6		; dig into the stack to find 'LOPVAR'
	bra	fr7
fr6:	add.l	#20,a6		; look at next stack frame
fr7:	move.l	(a6),d0 	; is it zero?
	beq	fr8		; if so, we're done
	cmp.l	lopvar,d0	; same as current LOPVAR?
	bne	fr6		; nope, look some more
	move.l	sp,a2		; Else remove 5 long words from...
	move.l	a6,a1		; inside the stack.
	lea	20,a3
	add.l	a1,a3
	bsr.l	mvdown
	move.l	a3,sp		; set the SP 5 long words up
fr8:	bra	finish		; and continue execution

next:	bsr.l	tstv		; get address of variable
	bcs.l	qwhat		; if no variable, say "What?"
	move.l	d0,a1		; save variable's address
nx0:	move.l	lopvar,d0	; If 'LOPVAR' is zero, we never...
	beq.l	qwhat		; had a FOR loop, so say "What?"
	cmp.l	d0,a1		; else we check them
	beq	nx3		; OK, they agree
	bsr.l	popa		; nope, let's see the next frame
	bra	nx0
nx3:	move.l	(a1),d0 	; get control variable's value
	add.l	lopinc,d0	; add in loop increment
	bvs.l	qhow		; say "How?" for 32-bit overflow
	move.l	d0,(a1) 	; save control variable's new value
	move.l	loplmt,d1	; get loop's limit value
	tst.l	lopinc
	bpl	nx1		; branch if loop increment is positive
	exg	d0,d1
nx1:	cmp.l	d0,d1		; test against limit
	blt	nx2		; branch if outside limit
	move.l	lopln,currnt	; Within limit, go back to the...
	move.l	loppt,a0	; saved 'CURRNT' and text pointer.
	bra	finish
nx2:	bsr.l	popa		; purge this loop
	bra	finish

;
;******************************************************************
;
; *** REM *** IF *** INPUT *** LET (& DEFLT) ***
;
; 'REM' can be followed by anything and is ignored by the
; interpreter.
;
; 'IF' is followed by an expression, as a condition and one or
; more commands (including other 'IF's) separated by colons.
; Note that the word 'THEN' is not used.  The interpreter evaluates
; the expression.  If it is non-zero, execution continues.  If it
; is zero, the commands that follow are ignored and execution
; continues on the next line.
;
; 'INPUT' is like the 'PRINT' command, and is followed by a list
; of items.  If the item is a string in single or double quotes,
; or is an underline (back arrow), it has the same effect as in
; 'PRINT'.  If an item is a variable, this variable name is
; printed out followed by a colon, then the interpreter waits for
; an expression to be typed in.  The variable is then set to the
; value of this expression.  If the variable is preceeded by a
; string (again in single or double quotes), the string will be
; displayed followed by a colon.  The interpreter the waits for an
; expression to be entered and sets the variable equal to the
; expression's value.  If the input expression is invalid, the
; interpreter will print "What?", "How?", or "Sorry" and reprint
; the prompt and redo the input.  The execution will not terminate
; unless you press control-C.  This is handled in 'INPERR'.
;
; 'LET' is followed by a list of items separated by commas.
; Each item consists of a variable, an equals sign, and an
; expression.  The interpreter evaluates the expression and sets
; the variable to that value.  The interpreter will also handle
; 'LET' commands without the word 'LET'.  This is done by 'DEFLT'.
;
rem:	bra	if2		; skip the rest of the line

if:	bsr.l	expr		; evaluate the expression
if1:	tst.l	d0		; is it zero?
	bne	runsml		; if not, continue
if2:	move.l	a0,a1
	clr.l	d1
	bsr.l	fndskp		; if so, skip the rest of the line
	bcc	runtsl		; and run the next line
	bra.l	wstart		; if no next line, do a warm start

inperr:	move.l	stkinp,sp	; restore the old stack pointer
	move.l	(sp)+,currnt	; and old 'CURRNT'
	addq.l	#4,sp
	move.l	(sp)+,a0	; and old text pointer

input:	move.l	a0,-(sp)	; save in case of error
	bsr.l	qtstg		; is next item a string?
	bra.s	ip2		; nope
	bsr.l	tstv		; yes, but is it followed by a variable?
	bcs	ip4		; if not, branch
	move.l	d0,a2		; put away the variable's address
	bra	ip3		; if so, input to variable
ip2:	move.l	a0,-(sp)	; save for 'PRTSTG'
	bsr.l	tstv		; must be a variable now
	bcs.l	qwhat		; "What?" it isn't?
	move.l	d0,a2		; put away the variable's address
	move.b	(a0),d2 	; get ready for 'PRTSTG'
	clr.b	d0
	move.b	d0,(a0)
	move.l	(sp)+,a1
	bsr.l	prtstg		; print string as prompt
	move.b	d2,(a0) 	; restore text
ip3:	move.l	a0,-(sp)	; save in case of error
	move.l	currnt,-(sp)	; also save 'CURRNT'
	move.l	#-1,currnt	; flag that we are in INPUT
	move.l	sp,stkinp	; save the stack pointer too
	move.l	a2,-(sp)	; save the variable address
	move.b	#':,d0		; print a colon first
	bsr.l	getln		; then get an input line
	lea	buffer,a0	; point to the buffer
	bsr.l	expr		; evaluate the input
	move.l	(sp)+,a2	; restore the variable address
	move.l	d0,(a2) 	; save value in variable
	move.l	(sp)+,currnt	; restore old 'CURRNT'
	move.l	(sp)+,a0	; and the old text pointer
ip4:	addq.l	#4,sp		; clean up the stack
	bsr.l	tstc		; is the next thing a comma?
	.byte	',,ip5-.
	bra	input		; yes, more items
ip5:	bra	finish

deflt:	cmp.b	#cr,(a0)	; empty line is OK
	beq	lt1		; else it is 'LET'

let:	bsr.l	setval		; do the assignment
	bsr.l	tstc		; check for more 'LET' items
	.byte	',,lt1-.
	bra	let
lt1:	bra	finish		; until we are finished.

;
;******************************************************************
;
; *** LOAD *** & SAVE ***
;
; These two commands transfer a program to/from an auxiliary
; device such as a cassette, another computer, etc.  The program
; is converted to an easily-stored format: each line starts with
; a colon, the line no. as 4 hex digits, and the rest of the line.
; At the end, a line starting with an '@' sign is sent.  This
; format can be read back with a minimum of processing time by
; the 68000.
;
load:	move.l	txtbgn,a0	; set pointer to start of prog. area
	move.b	#cr,d0		; For a CP/M host, tell it we're ready...
	bsr	goauxo		; by sending a CR to finish PIP command.
lod1:	bsr	goauxi		; look for start of line
	beq	lod1
	cmp.b	#'@,d0		; end of program?
	beq	lodend
	cmp.b	#':,d0		; if not, is it start of line?
	bne	lod1		; if not, wait for it
	bsr	gbyte		; get first byte of line no.
	move.b	d1,(a0)+	; store it
	bsr	gbyte		; get 2nd bye of line no.
	move.b	d1,(a0)+	; store that, too
lod2:	bsr	goauxi		; get another text char.
	beq	lod2
	move.b	d0,(a0)+	; store it
	cmp.b	#cr,d0		; is it the end of the line?
	bne	lod2		; if not, go back for more
	bra	lod1		; if so, start a new line
lodend:	move.l	a0,txtunf	; set end-of program pointer
	bra	wstart		; back to direct mode

gbyte:	moveq	#1,d2		; get two hex characters from auxiliary
	clr	d1		; and store them as a byte in D1
gbyte1:	bsr	goauxi		; get a char.
	beq	gbyte1
	cmp.b	#'A,d0
	bcs	gbyte2
	subq.b	#7,d0		; if greater than 9, adjust
gbyte2:	and.b	#0xf,d0		; strip ASCII
	lsl.b	#4,d1		; put nybble into the result
	or.b	d0,d1
	dbra	d2,gbyte1	; get another char.
	rts

save:	move.l	txtbgn,a0	; set pointer to start of prog. area
	move.l	txtunf,a1	; set pointer to end of prog. area
save1:	move.b	#cr,d0		; send out a CR & LF (CP/M likes this)
	bsr	goauxo
	move.b	#lf,d0
	bsr	goauxo
	cmp.l	a0,a1		; are we finished?
	bls	savend
	move.b	#':,d0		; if not, start a line
	bsr	goauxo
	move.b	(a0)+,d1	; send first half of line no.
	bsr	pbyte
	move.b	(a0)+,d1	; and send 2nd half
	bsr	pbyte
save2:	move.b	(a0)+,d0	; get a text char.
	cmp.b	#cr,d0		; is it the end of the line?
	beq	save1		; if so, send CR & LF and start new line
	bsr	goauxo		; send it out
	bra	save2		; go back for more text
savend:	move.b	#'@,d0		; send end-of-program indicator
	bsr	goauxo
	move.b	#cr,d0		; followed by a CR & LF
	bsr	goauxo
	move.b	#lf,d0
	bsr	goauxo
	move.b	#0x1a,d0 	; and a control-Z to end the CP/M file
	bsr	goauxo
	bra	wstart		; then go do a warm start

pbyte:	moveq	#1,d2		; send two hex characters from D1's low byte
pbyte1:	rol.b	#4,d1		; get the next nybble
	move.b	d1,d0
	and.b	#0xf,d0		; strip off garbage
	add.b	#'0,d0		; make it into ASCII
	cmp.b	#'9,d0
	bls	pbyte2
	addq.b	#7,d0		; adjust if greater than 9
pbyte2:	bsr	goauxo		; send it out
	dbra	d2,pbyte1	; then send the next nybble
	rts

;
;******************************************************************
;
; *** POKE *** & CALL ***
;
; 'POKE expr1,expr2' stores the byte from 'expr2' into the memory
; address specified by 'expr1'.
;
; 'CALL expr' jumps to the machine language subroutine whose
; starting address is specified by 'expr'.  The subroutine can use
; all registers but must leave the stack the way it found it.
; The subroutine returns to the interpreter by executing an RTS.
;
poke:	bsr	expr		; get the memory address
	bsr.l	tstc		; it must be followed by a comma
	.byte	',,pker-.
	move.l	d0,-(sp)	; save the address
	bsr	expr		; get the byte to be POKE'd
	move.l	(sp)+,a1	; get the address back
	move.b	d0,(a1) 	; store the byte in memory
	bra	finish
pker:	bra.l	qwhat		; if no comma, say "What?"

call:	bsr	expr		; get the subroutine's address
	tst.l	d0		; make sure we got a valid address
	beq.l	qhow		; if not, say "How?"
	move.l	a0,-(sp)	; save the text pointer
	move.l	d0,a1
	jsr	(a1)		; jump to the subroutine
	move.l	(sp)+,a0	; restore the text pointer
	bra	finish
;
;******************************************************************
;
; *** EXPR ***
;
; 'EXPR' evaluates arithmetical or logical expressions.
; <EXPR>::=<EXPR2>
;	   <EXPR2><rel.op.><EXPR2>
; where <rel.op.> is one of the operators in TAB8 and the result
; of these operations is 1 if true and 0 if false.
; <EXPR2>::=(+ or -)<EXPR3>(+ or -)<EXPR3>(...
; where () are optional and (... are optional repeats.
; <EXPR3>::=<EXPR4>( <* or /><EXPR4> )(...
; <EXPR4>::=<variable>
;	    <function>
;	    (<EXPR>)
; <EXPR> is recursive so that the variable '@' can have an <EXPR>
; as an index, functions can have an <EXPR> as arguments, and
; <EXPR4> can be an <EXPR> in parenthesis.
;
expr:	bsr	expr2
	move.l	d0,-(sp)	; save <EXPR2> value
	lea	tab8,a1 	; look up a relational operator
	lea	tab8.1,a2
	bra	exec		; go do it

xp11:	bsr	xp18		; is it ">="?
	blt	xprt0		; no, return D0=0
	bra	xprt1		; else return D0=1

xp12:	bsr	xp18		; is it "<>"?
	beq	xprt0		; no, return D0=0
	bra	xprt1		; else return D0=1

xp13:	bsr	xp18		; is it ">"?
	ble	xprt0		; no, return D0=0
	bra	xprt1		; else return D0=1

xp14:	bsr	xp18		; is it "<="?
	bgt	xprt0		; no, return D0=0
	bra	xprt1		; else return D0=1

xp15:	bsr	xp18		; is it "="?
	bne	xprt0		; if not, return D0=0
	bra	xprt1		; else return D0=1
xp15rt:	rts

xp16:	bsr	xp18		; is it "<"?
	bge	xprt0		; if not, return D0=0
	bra	xprt1		; else return D0=1
xp16rt:	rts

xprt0:	clr.l	d0		; return D0=0 (false)
	rts

xprt1:	moveq	#1,d0		; return D0=1 (true)
	rts

xp17:	move.l	(sp)+,d0	; it's not a rel. operator
	rts			; return D0=<EXPR2>

xp18:	move.l	(sp)+,d0	; reverse the top two stack items
	move.l	(sp)+,d1
	move.l	d0,-(sp)
	move.l	d1,-(sp)
	bsr	expr2		; do second <EXPR2>
	move.l	(sp)+,d1
	cmp.l	d0,d1		; compare with the first result
	rts			; return the result

expr2:	bsr.l	tstc		; negative sign?
	.byte	'-,xp21-.
	clr.l	d0		; yes, fake '0-'
	bra	xp26
xp21:	bsr.l	tstc		; positive sign? ignore it
	.byte	'+,xp22-.
xp22:	bsr	expr3		; first <EXPR3>
xp23:	bsr.l	tstc		; add?
	.byte	'+,xp25-.
	move.l	d0,-(sp)	; yes, save the value
	bsr	expr3		; get the second <EXPR3>
xp24:	move.l	(sp)+,d1
	add.l	d1,d0		; add it to the first <EXPR3>
	bvs.l	qhow		; branch if there's an overflow
	bra	xp23		; else go back for more operations
xp25:	bsr.l	tstc		; subtract?
	.byte	'-,xp42-.
xp26:	move.l	d0,-(sp)	; yes, save the result of 1st <EXPR3>
	bsr	expr3		; get second <EXPR3>
	neg.l	d0		; change its sign
	jmp	xp24		; and do an addition

expr3:	bsr	expr4		; get first <EXPR4>
xp31:	bsr.l	tstc		; multiply?
	.byte	'*,xp34-.
	move.l	d0,-(sp)	; yes, save that first result
	bsr	expr4		; get second <EXPR4>
	move.l	(sp)+,d1
	bsr.l	mult32		; multiply the two
	bra	xp31		; then look for more terms
xp34:	bsr.l	tstc		; divide?
	.byte	'/,xp42-.
	move.l	d0,-(sp)	; save result of 1st <EXPR4>
	bsr	expr4		; get second <EXPR4>
	move.l	(sp)+,d1
	exg	d0,d1
	bsr.l	div32		; do the division
	bra	xp31		; go back for any more terms

expr4:	lea	tab4,a1 	; find possible function
	lea	tab4.1,a2
	bra	exec
xp40:	bsr	tstv		; nope, not a function
	bcs	xp41		; nor a variable
	move.l	d0,a1
	clr.l	d0
	move.l	(a1),d0 	; if a variable, return its value in D0
exp4rt:	rts
xp41:	bsr.l	tstnum		; or is it a number?
	move.l	d1,d0
	tst	d2		; (if not, # of digits will be zero)
	bne	exp4rt		; if so, return it in D0
parn:	bsr.l	tstc		; else look for ( EXPR )
	.byte	'(,xp43-.
	bsr	expr
	bsr.l	tstc
	.byte	'),xp43-.
xp42:	rts
xp43:	bra.l	qwhat		; else say "What?"

;
; ===== Test for a valid variable name.  Returns Carry=1 if not
;	found, else returns Carry=0 and the address of the
;	variable in D0.

tstv:	bsr.l	ignblk
	clr.l	d0
	move.b	(a0),d0 	; look at the program text
	sub.b	#'@,d0
	bcs	tstvrt		; C=1: not a variable
	bne	tv1		; branch if not "@" array
	addq	#1,a0		; If it is, it should be
	bsr	parn		; followed by (EXPR) as its index.
	add.l	d0,d0
	bcs.l	qhow		; say "How?" if index is too big
	add.l	d0,d0
	bcs.l	qhow
	move.l	d0,-(sp)	; save the index
	bsr.l	size		; get amount of free memory
	move.l	(sp)+,d1	; get back the index
	cmp.l	d1,d0		; see if there's enough memory
	bls.l	qsorry		; if not, say "Sorry"
	move.l	varbgn,d0	; put address of array element...
	sub.l	d1,d0		; into D0
	rts
tv1:	cmp.b	#27,d0		; if not @, is it A through Z?
	eor	#1,ccr
	bcs	tstvrt		; if not, set Carry and return
	addq	#1,a0		; else bump the text pointer
	add	d0,d0		; compute the variable's address
	add	d0,d0
	move.l	varbgn,d1
	add	d1,d0		; and return it in D0 with Carry=0
tstvrt:	rts

;
; ===== Multiplies the 32 bit values in D0 and D1, returning
;	the 32 bit result in D0.
;
mult32:	move.l	d1,d4
	eor.l	d0,d4		; see if the signs are the same
	tst.l	d0		; take absolute value of D0
	bpl	mlt1
	neg.l	d0
mlt1:	tst.l	d1		; take absolute value of D1
	bpl	mlt2
	neg.l	d1
mlt2:	cmp.l	#0xffff,d1	; is second argument <= 16 bits?
	bls	mlt3		; OK, let it through
	exg	d0,d1		; else swap the two arguments
	cmp.l	#0xffff,d1	; and check 2nd argument again
	bhi.l	qhow		; one of them MUST be 16 bits
mlt3:	move	d0,d2		; prepare for 32 bit X 16 bit multiply
	mulu	d1,d2		; multiply low word
	swap	d0
	mulu	d1,d0		; multiply high word
	swap	d0
;** Rick Murray's bug correction follows:
	tst	d0		; if lower word not 0, then overflow
	bne.l	qhow		; if overflow, say "How?"
	add.l	d2,d0		; D0 now holds the product
	bmi.l	qhow		; if sign bit set, it's an overflow
	tst.l	d4		; were the signs the same?
	bpl	mltret
	neg.l	d0		; if not, make the result negative
mltret:	rts

;
; ===== Divide the 32 bit value in D0 by the 32 bit value in D1.
;	Returns the 32 bit quotient in D0, remainder in D1.
;
div32:	tst.l	d1		; check for divide-by-zero
	beq.l	qhow		; if so, say "How?"
	move.l	d1,d2
	move.l	d1,d4
	eor.l	d0,d4		; see if the signs are the same
	tst.l	d0		; take absolute value of D0
	bpl	div1
	neg.l	d0
div1:	tst.l	d1		; take absolute value of D1
	bpl	div2
	neg.l	d1
div2:	moveq	#31,d3		; iteration count for 32 bits
	move.l	d0,d1
	clr.l	d0
div3:	add.l	d1,d1		; (This algorithm was translated from
	addx.l	d0,d0		; the divide routine in Ron Cain's
	beq	div4		; Small-C run time library.)
	cmp.l	d2,d0
	bmi	div4
	addq.l	#1,d1
	sub.l	d2,d0
div4:	dbra	d3,div3
	exg	d0,d1		; put rem. & quot. in proper registers
	tst.l	d4		; were the signs the same?
	bpl	divrt
	neg.l	d0		; if not, results are negative
	neg.l	d1
divrt:	rts

;
; ===== The PEEK function returns the byte stored at the address
;	contained in the following expression.
;
peek:	bsr	parn		; get the memory address
	move.l	d0,a1
	clr.l	d0		; upper 3 bytes will be zero
	move.b	(a1),d0 	; get the addressed byte
	rts			; and return it

;
; ===== The RND function returns a random number from 1 to
;	the value of the following expression in D0.
;
rnd:	bsr	parn		; get the upper limit
	tst.l	d0		; it must be positive and non-zero
	beq.l	qhow
	bmi.l	qhow
	move.l	d0,d1
	move.l	ranpnt,a1	; get memory as a random number
	cmp.l	#lstrom,a1
	bcs	ra1
	lea	start,a1	; wrap around if end of program
ra1:	move.l	(a1)+,d0	; get the slightly random number
	bclr	#31,d0		; make sure it's positive
	move.l	a1,ranpnt	; (even I can do better than this!)
	bsr	div32		; RND(n)=MOD(number,n)+1
	move.l	d1,d0		; MOD is the remainder of the div.
	addq.l	#1,d0
	rts

;
; ===== The ABS function returns an absolute value in D0.
;
abs:	bsr	parn		; get the following expr.'s value
	tst.l	d0
	bpl	absrt
	neg.l	d0		; if negative, complement it
	bmi.l	qhow		; if still negative, it was too big
absrt:	rts

;
; ===== The SIZE function returns the size of free memory in D0.
;
size:	move.l	varbgn,d0	; get the number of free bytes...
	sub.l	txtunf,d0	; between 'TXTUNF' and 'VARBGN'
	rts			; return the number in D0

;
;******************************************************************
;
; *** SETVAL *** FIN *** ENDCHK *** ERROR (& friends) ***
;
; 'SETVAL' expects a variable, followed by an equal sign and then
; an expression.  It evaluates the expression and sets the variable
; to that value.
;
; 'FIN' checks the end of a command.  If it ended with ":",
; execution continues.	If it ended with a CR, it finds the
; the next line and continues from there.
;
; 'ENDCHK' checks if a command is ended with a CR. This is
; required in certain commands, such as GOTO, RETURN, STOP, etc.
;
; 'ERROR' prints the string pointed to by A0. It then prints the
; line pointed to by CURRNT with a "?" inserted at where the
; old text pointer (should be on top of the stack) points to.
; Execution of Tiny BASIC is stopped and a warm start is done.
; If CURRNT is zero (indicating a direct command), the direct
; command is not printed. If CURRNT is -1 (indicating
; 'INPUT' command in progress), the input line is not printed
; and execution is not terminated but continues at 'INPERR'.
;
; Related to 'ERROR' are the following:
; 'QWHAT' saves text pointer on stack and gets "What?" message.
; 'AWHAT' just gets the "What?" message and jumps to 'ERROR'.
; 'QSORRY' and 'ASORRY' do the same kind of thing.
; 'QHOW' and 'AHOW' also do this for "How?".
;
setval:	bsr	tstv		; variable name?
	bcs	qwhat		; if not, say "What?"
	move.l	d0,-(sp)	; save the variable's address
	bsr.l	tstc		; get past the "=" sign
	.byte	'=,sv1-.
	bsr	expr		; evaluate the expression
	move.l	(sp)+,a6
	move.l	d0,(a6) 	; and save its value in the variable
	rts
sv1:	bra	qwhat		; if no "=" sign

fin:	bsr.l	tstc		; *** FIN ***
	.byte	':,fi1-.
	addq.l	#4,sp		; if ":", discard return address
	bra	runsml		; continue on the same line
fi1:	bsr.l	tstc		; not ":", is it a CR?
	.byte	cr,fi2-.
	addq.l	#4,sp		; yes, purge return address
	bra	runnxl		; execute the next line
fi2:	rts			; else return to the caller

endchk:	bsr.l	ignblk
	cmp.b	#cr,(a0)	; does it end with a CR?
	bne	qwhat		; if not, say "WHAT?"
	rts

qwhat:	move.l	a0,-(sp)
awhat:	lea	whtmsg,a6
error:	bsr.l	prmesg		; display the error message
	move.l	(sp)+,a0	; restore the text pointer
	move.l	currnt,d0	; get the current line number
	beq	wstart		; if zero, do a warm start
	cmp.l	#-1,d0		; is the line no. pointer = -1?
	beq	inperr		; if so, redo input
	move.b	(a0),-(sp)	; save the char. pointed to
	clr.b	(a0)		; put a zero where the error is
	move.l	currnt,a1	; point to start of current line
	bsr.l	prtln		; display the line in error up to the 0
	move.b	(sp)+,(a0)	; restore the character
	move.b	#'?,d0		; display a "?"
	bsr	goout
	clr	d0
	subq.l	#1,a1		; point back to the error char.
	bsr.l	prtstg		; display the rest of the line
	bra	wstart		; and do a warm start
qsorry:	move.l	a0,-(sp)
asorry:	lea	srymsg,a6
	bra	error
qhow:	move.l	a0,-(sp)	; Error: "How?"
ahow:	lea	howmsg,a6
	bra	error
;
;******************************************************************
;
; *** GETLN *** FNDLN (& friends) ***
;
; 'GETLN' reads in input line into 'BUFFER'. It first prompts with
; the character in D0 (given by the caller), then it fills the
; buffer and echos. It ignores LF's but still echos
; them back. Control-H is used to delete the last character
; entered (if there is one), and control-X is used to delete the
; whole line and start over again. CR signals the end of a line,
; and causes 'GETLN' to return.
;
; 'FNDLN' finds a line with a given line no. (in D1) in the
; text save area.  A1 is used as the text pointer. If the line
; is found, A1 will point to the beginning of that line
; (i.e. the high byte of the line no.), and flags are NC & Z.
; If that line is not there and a line with a higher line no.
; is found, A1 points there and flags are NC & NZ. If we reached
; the end of the text save area and cannot find the line, flags
; are C & NZ.
; 'FNDLN' will initialize A1 to the beginning of the text save
; area to start the search. Some other entries of this routine
; will not initialize A1 and do the search.
; 'FNDLNP' will start with A1 and search for the line no.
; 'FNDNXT' will bump A1 by 2, find a CR and then start search.
; 'FNDSKP' uses A1 to find a CR, and then starts the search.
;
getln:	bsr	goout		; display the prompt
	move.b	#' ,d0		; and a space
	bsr	goout
	lea	buffer,a0	; A0 is the buffer pointer
gl1:	bsr.l	chkio		; check keyboard
	beq	gl1		; wait for a char. to come in
	cmp.b	#ctrlh,d0	; delete last character?
	beq	gl3		; if so
	cmp.b	#ctrlx,d0	; delete the whole line?
	beq	gl4		; if so
	cmp.b	#cr,d0		; accept a CR
	beq	gl2
	cmp.b	#' ,d0		; if other control char., discard it
	bcs	gl1
gl2:	move.b	d0,(a0)+	; save the char.
	bsr	goout		; echo the char back out
	cmp.b	#cr,d0		; if it's a CR, end the line
	beq	gl7
	cmp.l	#(buffer+buflen-1),a0	; any more room?
	bcs	gl1		; yes: get some more, else delete last char.
gl3:	move.b	#ctrlh,d0	; delete a char. if possible
	bsr	goout
	move.b	#' ,d0
	bsr	goout
	cmp.l	#buffer,a0	; any char.'s left?
	bls	gl1		; if not
	move.b	#ctrlh,d0	; if so, finish the BS-space-BS sequence
	bsr	goout
	subq.l	#1,a0		; decrement the text pointer
	bra	gl1		; back for more
gl4:	move.l	a0,d1		; delete the whole line
	sub.l	#buffer,d1	; figure out how many backspaces we need
	beq	gl6		; if none needed, branch
	subq	#1,d1		; adjust for DBRA
gl5:	move.b	#ctrlh,d0	; and display BS-space-BS sequences
	bsr	goout
	move.b	#' ,d0
	bsr	goout
	move.b	#ctrlh,d0
	bsr	goout
	dbra	d1,gl5
gl6:	lea	buffer,a0	; reinitialize the text pointer
	bra	gl1		; and go back for more
gl7:	move.b	#lf,d0		; echo a LF for the CR
	bsr	goout
	rts

fndln:	cmp.l	#0xffff,d1	; line no. must be < 65535
	bcc	qhow
	move.l	txtbgn,a1	; init. the text save pointer

fndlnp:	move.l	txtunf,a2	; check if we passed the end
	subq.l	#1,a2
	cmp	a1,a2
	bcs	fndret		; if so, return with Z=0 & C=1
	move.b	(a1)+,d2	; if not, get a line no.
	lsl	#8,d2
	move.b	(a1),d2
	subq.l	#1,a1
	cmp	d1,d2		; is this the line we want?
	bcs	fndnxt		; no, not there yet
fndret:	rts			; return the cond. codes

fndnxt:	addq.l	#2,a1		; find the next line

fndskp:	cmp.b	#cr,(a1)+	; try to find a CR
	bne	fndskp		; keep looking
	bra	fndlnp		; check if end of text

;
;******************************************************************
;
; *** MVUP *** MVDOWN *** POPA *** PUSHA ***
;
; 'MVUP' moves a block up from where A1 points to where A2 points
; until A1=A3
;
; 'MVDOWN' moves a block down from where A1 points to where A3
; points until A1=A2
;
; 'POPA' restores the 'FOR' loop variable save area from the stack
;
; 'PUSHA' stacks for 'FOR' loop variable save area onto the stack
;
mvup:	cmp.l	a1,a3		; see the above description
	beq	mvret
	move.b	(a1)+,(a2)+
	bra	mvup
mvret:	rts

mvdown:	cmp.l	a1,a2		; see the above description
	beq	mvret
	move.b	-(a1),-(a3)
	bra	mvdown

popa:	move.l	(sp)+,a6	; A6 = return address
	move.l	(sp)+,lopvar	; restore LOPVAR, but zero means no more
	beq	pp1
	move.l	(sp)+,lopinc	; if not zero, restore the rest
	move.l	(sp)+,loplmt
	move.l	(sp)+,lopln
	move.l	(sp)+,loppt
pp1:	jmp	(a6)		; return

pusha:	move.l	stklmt,d1	; Are we running out of stack room?
	sub.l	sp,d1
	bcc	qsorry		; if so, say we're sorry
	move.l	(sp)+,a6	; else get the return address
	move.l	lopvar,d1	; save loop variables
	beq	pu1		; if LOPVAR is zero, that's all
	move.l	loppt,-(sp)	; else save all the others
	move.l	lopln,-(sp)
	move.l	loplmt,-(sp)
	move.l	lopinc,-(sp)
pu1:	move.l	d1,-(sp)
	jmp	(a6)		; return

;
;******************************************************************
;
; *** PRTSTG *** QTSTG *** PRTNUM *** PRTLN ***
;
; 'PRTSTG' prints a string pointed to by A1. It stops printing
; and returns to the caller when either a CR is printed or when
; the next byte is the same as what was passed in D0 by the
; caller.
;
; 'QTSTG' looks for an underline (back-arrow on some systems),
; single-quote, or double-quote.  If none of these are found, returns
; to the caller.  If underline, outputs a CR without a LF.  If single
; or double quote, prints the quoted string and demands a matching
; end quote.  After the printing, the next 2 bytes of the caller are
; skipped over (usually a short branch instruction).
;
; 'PRTNUM' prints the 32 bit number in D1, leading blanks are added if
; needed to pad the number of spaces to the number in D4.
; However, if the number of digits is larger than the no. in
; D4, all digits are printed anyway. Negative sign is also
; printed and counted in, positive sign is not.
;
; 'PRTLN' prints the saved text line pointed to by A1
; with line no. and all.
;
prtstg:	move.b	d0,d1		; save the stop character
ps1:	move.b	(a1)+,d0	; get a text character
	cmp.b	d0,d1		; same as stop character?
	beq	prtret		; if so, return
	bsr	goout		; display the char.
	cmp.b	#cr,d0		; is it a C.R.?
	bne	ps1		; no, go back for more
	move.b	#lf,d0		; yes, add a L.F.
	bsr	goout
prtret:	rts			; then return

qtstg:	bsr.l	tstc		; *** QTSTG ***
	.byte	'",qt3-.
	move.b	#'",d0		; it is a "
qt1:	move.l	a0,a1
	bsr	prtstg		; print until another
	move.l	a1,a0
	move.l	(sp)+,a1	; pop return address
	cmp.b	#lf,d0		; was last one a CR?
	beq	runnxl		; if so, run next line
qt2:	addq.l	#2,a1		; skip 2 bytes on return
	jmp	(a1)		; return
qt3:	bsr.l	tstc		; is it a single quote?
	.byte	'',qt4-.
	move.b	#'',d0	; if so, do same as above
	bra	qt1
qt4:	bsr.l	tstc		; is it an underline?
	.byte	'_,qt5-.
	move.b	#cr,d0		; if so, output a CR without LF
	bsr.l	goout
	move.l	(sp)+,a1	; pop return address
	bra	qt2
qt5:	rts			; none of the above

prtnum:	move.l	d1,d3		; save the number for later
	move	d4,-(sp)	; save the width value
	move.b	#0xff,-(sp)	; flag for end of digit string
	tst.l	d1		; is it negative?
	bpl	pn1		; if not
	neg.l	d1		; else make it positive
	subq	#1,d4		; one less for width count
pn1:	divu	#10,d1		; get the next digit
	bvs	pnov		; overflow flag set?
	move.l	d1,d0		; if not, save remainder
	and.l	#0xffff,d1	; strip the remainder
	bra	toascii 	; skip the overflow stuff
pnov:	move	d1,d0		; prepare for long word division
	clr.w	d1		; zero out low word
	swap	d1		; high word into low
	divu	#10,d1		; divide high word
	move	d1,d2		; save quotient
	move	d0,d1		; low word into low
	divu	#10,d1		; divide low word
	move.l	d1,d0		; D0 = remainder
	swap	d1		; R/Q becomes Q/R
	move	d2,d1		; D1 is low/high
	swap	d1		; D1 is finally high/low
toascii:
	swap	d0		; get remainder
	move.b	d0,-(sp)	; stack it as a digit
	swap	d0
	subq	#1,d4		; decrement width count
	tst.l	d1		; if quotient is zero, we're done
	bne	pn1
	subq	#1,d4		; adjust padding count for DBRA
	bmi	pn4		; skip padding if not needed
pn3:	move.b	#' ,d0		; display the required leading spaces
	bsr	goout
	dbra	d4,pn3
pn4:	tst.l	d3		; is number negative?
	bpl	pn5
	move.b	#'-,d0		; if so, display the sign
	bsr	goout
pn5:	move.b	(sp)+,d0	; now unstack the digits and display
	bmi	pnret		; until the flag code is reached
	add.b	#'0,d0		; make into ASCII
	bsr	goout
	bra	pn5
pnret:	move	(sp)+,d4	; restore width value
	rts

prtln:	clr.l	d1
	move.b	(a1)+,d1	; get the binary line number
	lsl	#8,d1
	move.b	(a1)+,d1
	moveq	#5,d4		; display a 5 digit line no.
	bsr	prtnum
	move.b	#' ,d0		; followed by a blank
	bsr	goout
	clr	d0		; stop char. is a zero
	bra	prtstg		; display the rest of the line

;
; ===== Test text byte following the call to this subroutine. If it
;	equals the byte pointed to by A0, return to the code following
;	the call. If they are not equal, branch to the point
;	indicated by the offset byte following the text byte.
;
tstc:	bsr	ignblk		; ignore leading blanks
	move.l	(sp)+,a1	; get the return address
	move.b	(a1)+,d1	; get the byte to compare
	cmp.b	(a0),d1 	; is it = to what A0 points to?
	beq	tc1		; if so
	clr.l	d1		; If not, add the second
	move.b	(a1),d1 	; byte following the call to
	add.l	d1,a1		; the return address.
	jmp	(a1)		; jump to the routine
tc1:	addq.l	#1,a0		; if equal, bump text pointer
	addq.l	#1,a1		; Skip the 2 bytes following
	jmp	(a1)		; the call and continue.

;
; ===== See if the text pointed to by A0 is a number. If so,
;	return the number in D1 and the number of digits in D2,
;	else return zero in D1 and D2.
;
tstnum:	clr.l	d1		; initialize return parameters
	clr	d2
	bsr	ignblk		; skip over blanks
tn1:	cmp.b	#'0,(a0)	; is it less than zero?
	bcs	tsnmret 	; if so, that's all
	cmp.b	#'9,(a0)	; is it greater than nine?
	bhi	tsnmret 	; if so, return
	cmp.l	#214748364,d1	; see if there's room for new digit
	bcc	qhow		; if not, we've overflowd
	move.l	d1,d0		; quickly multiply result by 10
	add.l	d1,d1
	add.l	d1,d1
	add.l	d0,d1
	add.l	d1,d1
	move.b	(a0)+,d0	; add in the new digit
	and.l	#0xf,d0
	add.l	d0,d1
	addq	#1,d2		; increment the no. of digits
	bra	tn1
tsnmret:
	rts

;
; ===== Skip over blanks in the text pointed to by A0.
;
ignblk:	cmp.b	#' ,(a0)	; see if it's a space
	bne	igbret		; if so, swallow it
igb1:	addq.l	#1,a0		; increment the text pointer
	bra	ignblk
igbret:	rts

;
; ===== Convert the line of text in the input buffer to upper
;	case (except for stuff between quotes).
;
toupbuf:
	lea	buffer,a0	; set up text pointer
	clr.b	d1		; clear quote flag
toupb1:	move.b	(a0)+,d0	; get the next text char.
	cmp.b	#cr,d0		; is it end of line?
	beq	toupbrt 	; if so, return
	cmp.b	#'",d0		; a double quote?
	beq	doquo
	cmp.b	#'',d0		; or a single quote?
	beq	doquo
	tst.b	d1		; inside quotes?
	bne	toupb1		; if so, do the next one
	bsr	toupper 	; convert to upper case
	move.b	d0,-(a0)	; store it
	addq.l	#1,a0
	bra	toupb1		; and go back for more
toupbrt:
	rts

doquo:	tst.b	d1		; are we inside quotes?
	bne	doquo1
	move.b	d0,d1		; if not, toggle inside-quotes flag
	bra	toupb1
doquo1:	cmp.b	d0,d1		; make sure we're ending proper quote
	bne	toupb1		; if not, ignore it
	clr.b	d1		; else clear quote flag
	bra	toupb1

;
; ===== Convert the character in D0 to upper case
;
toupper:
	cmp.b	#'a,d0		; is it < 'a'?
	bcs	toupret
	cmp.b	#'z,d0		; or > 'z'?
	bhi	toupret
	sub.b	#32,d0		; if not, make it upper case
toupret:
	rts

;
; 'CHKIO' checks the input. If there's no input, it will return
; to the caller with the Z flag set. If there is input, the Z
; flag is cleared and the input byte is in D0. However, if a
; control-C is read, 'CHKIO' will warm-start BASIC and will not
; return to the caller.
;
chkio:	bsr.l	goin		; get input if possible
	beq	chkret		; if Zero, no input
	cmp.b	#ctrlc,d0	; is it control-C?
	bne	chkret		; if not
	bra.l	wstart		; if so, do a warm start
chkret:	rts

;
; ===== Display a CR-LF sequence
;
crlf:	lea	clmsg,a6

;
; ===== Display a zero-ended string pointed to by register A6
;
prmesg:	move.b	(a6)+,d0	; get the char.
	beq	prmret		; if it's zero, we're done
	bsr	goout		; else display it
	bra	prmesg
prmret:	rts

;*****************************************************
; The following routines are the only ones that need *
; to be changed for a different I/O environment.     *
;*****************************************************

;
; ===== Output character to the console (Port 1) from register D0
;	(Preserves all registers.)
;
outc:	btst	#1,0x10040	; is port 1 ready for a character?
	beq	outc		; if not, wait for it
	move.b	d0,0x10042	; out it goes.
	rts

;
; ===== Input a character from the console into register D0 (or
;	return Zero status if there's no character available).
;
inc:	btst	#0,0x10040	; is character ready?
	beq	incret		; if not, return Zero status
	move.b	0x10042,d0	; else get the character
	and.b	#0x7f,d0 	; zero out the high bit
incret:	rts

;
; ===== Output character to the host (Port 2) from register D0
;	(Preserves all registers.)
;
auxout:	btst	#1,0x10041	; is port 2 ready for a character?
	beq	auxout		; if not, wait for it
	move.b	d0,0x10043	; out it goes.
	rts

;
; ===== Input a character from the host into register D0 (or
;	return Zero status if there's no character available).
;
auxin:	btst	#0,0x10041	; is character ready?
	beq	axiret		; if not, return Zero status
	move.b	0x10043,d0	; else get the character
	and.b	#0x7f,d0 	; zero out the high bit
axiret:	rts

;
; ===== Return to the resident monitor, operating system, etc.
;
byebye:	move.b	#228,d7 	; return to Tutor
	trap	#14

initmsg:
	.byte	cr,lf,'G,'o,'r,'d,'o,'','s,' ,'M,'C,'6,'8,'0,'0,'0,' ,'T,'i,'n,'y,' ,'B,'A,'S,'I,'C,',,' ,'v,'1,'.,'2,cr,lf,lf,0
okmsg:	.byte	cr,lf,'O,'K,cr,lf,0
howmsg:	.byte	'H,'o,'w,'?,cr,lf,0
whtmsg:	.byte	'W,'h,'a,'t,'?,cr,lf,0
srymsg:	.byte	'S,'o,'r,'r,'y,'.
clmsg:	.byte	cr,lf,0
	.byte	0	; <- for aligning on a word boundary
lstrom	=	.		; end of possible ROM area
;
; Internal variables follow:
;
ranpnt:	.long	start		; random number pointer
currnt:	.blkl	1		; Current line pointer
stkgos:	.blkl	1		; Saves stack pointer in 'GOSUB'
stkinp:	.blkl	1		; Saves stack pointer during 'INPUT'
lopvar:	.blkl	1		; 'FOR' loop save area
lopinc:	.blkl	1		; increment
loplmt:	.blkl	1		; limit
lopln:	.blkl	1		; line number
loppt:	.blkl	1		; text pointer
txtunf:	.blkl	1		; points to unfilled text area
varbgn:	.blkl	1		; points to variable area
stklmt:	.blkl	1		; holds lower limit for stack growth
buffer:	.blkb	buflen		; Keyboard input buffer
txt	=	.		; Beginning of program area
	.end
