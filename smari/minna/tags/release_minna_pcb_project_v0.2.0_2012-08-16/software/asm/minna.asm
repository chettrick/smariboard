;
.FILL	0800H, 0FFH		;(Required to trick assembler into
;						filling empty space with FFh bytes)

P8255	.EQU	03H		;8255 I control port
DIGIT	.EQU	02H		;8255 I port C
SEG7	.EQU	01H		;8255 I port B
KIN		.EQU	00H		;8255 I port A
PWCODE	.EQU	0A5H	;Power-up code
ZSUM	.EQU	71H		;This will make the sum of all
						;monitor codes to be zero

; The following EQUATEs are used for timing.
; Their values depend on the CPU clock frequency.
; (In this version, the crystal frequency is 1.79 MHz.)

COLDEL	.EQU	201		;Column delay time for routine
						;SCAN and SCAN1
F1KHZ	.EQU	65		;Delay count for 1 kHz square wave,
						;used by routine TONE1K.
F2KHZ	.EQU	31		;Delay count for 2 kHz square wave,
						;used by routine TONE2K.
MPERIOD .EQU	42		;1 kHz and 2 kHz threshold, used by
						;tape input routine PERIOD.

; The following EQUATEs are for tape modulation.
; If the quality of tape recorder is good, the user may
; change '4 4 2 8' to '2 2 1 4'. This will double
; the tape data rate.
; If the quality of tape recorder is poor, the user may
; change '4 4 2 8' to '6 6 3 12'. This will improve
; error performance but slow down the data rate.
; Although the data format is changed the tape is still
; compatible in each case, because only the ratio is
; detected in the Tape-read.

ONE_1K	.EQU	4
ONE_2K	.EQU	4
ZERO_1K	.EQU	2
ZERO_2K	.EQU	8

;***********************************************************
; I/O port assignment: (8255 I)

; port A (address 00H):
;	bit 7	-- tape input
;	bit 6	-- 'USER KEY' on keyboard, active low
;	bit 5-0	-- row of keyboard matrix input, active low
; port B (address 01H): 7 segments of LED, active high
;	bit 7	-- segment d
;	bit 6	-- decimal point
;	bit 5	-- segment c
;	bit 4	-- segment b
;	bit 3	-- segment a
;	bit 2	-- segment f
;	bit 1	-- segment g
;	bit 0	-- segment e
; port C (address 02H):
;	bit 7	-- tape & tone output
;	bit 6	-- BREAK enable. NMI (CPU pin 17) will go to
;			low 5 M1's (machine cycle one) after this
;			bit goes to low. (This bit is connected to
;			the reset input of external counter.)
;	bit 5-0	-- colums of keyboard and the display matrix,
;			active high. Bit 5 is the leftmost column.

;***********************************************************
RST00	.ORG	00H		;Required to make sure assembler 
;						doesn't skip over address 0000H
; There are two cases that will generate a RESET signal:
;	(i)  power-up
;	(ii) 'RS' key pressed
; In both cases, the following actions will be taken:
;   a) disable interrupt, set interrupt mode to 0
;      set I register to 00 and start execution
;      at address 0000 (by Z80 CPU itself).
;   b) initialize user's PC to the lowest RAM address;
;   c) set user's SP to FF9FH;
;   d) set user's I register to 00 and disable user's
;      interrupt flip-flop;
; In addition subroutine INI will be called on power-up
; reset, which has the following effects:
;   e) disable BREAK POINT;
;   f) set the contents of location FFEEH and FFEFH to 66
;      and 00 respectively. This will make instruction RST
;      38H (opcode FF) have the same effect as BREAK.
; Memory location POWERUP is used to distinguish power-up
; from RS-key.  (POWERUP) contains random data when
; power-up and contains PWCODE (0A5H) thereafter.

	LD	B, 0
	DJNZ	$	;Power-up delay

; Initialize 8255 to mode 0 with port A input, port B and C
; output. The control word is 90H.

	LD	A, 10010000B
	OUT	(P8255), A

; When the control word is sent to 8255, all output
; ports are cleared to 0.  It is necessary to disable
; BREAK and deactivate all I/O by sending 0C0H to
; port C.

	LD	A, 0C0H
	OUT	(DIGIT), A
	LD	SP, SYSSTK	;initial system stack

; If the content of location POWERUP is not equal to
; PWCODE, call subroutine INI. Continue otherwise.

	LD	A, (POWERUP)
	CP	PWCODE
	CALL	NZ, INI

; Determine the lowest RAM address by checking whether
; address 1000H is RAM.  If yes, set user's PC to this
; value.  Otherwise, set it to 8000H.

	LD	HL, 1000H
	CALL	RAMCHK
	JR	Z, PREPC
	LD	H, 80H
	
PREPC	LD	(USERPC), HL
	LD	H, 0

; Address 28H and 30H are reserved for BREAK (RST 28H)
; and software BREAK (RST 30H).  Skip these area, monitor
; program resumes at RESET1.

	JR	RESET1
;
;***********************************************************
RST28	.ORG	28H
; Address 28H is the entry point of BREAK trap.
; If a location is set as a BREAK point, the monitor
; will change the content of this location to C7 (RST 28H)
; before transfering control to user's program.
; In execution of user's program, a trap will occur if
; user's PC passes this location.  The monitor then takes
; over control and the content of BREAK address
; will be restored.  Monitor takes care of everything
; and makes the whole mechanism transparant to the user.
; The return address pushed onto stack is the PC after
; executing RST 28H.  The original break address should
; be one less than that.  The following 3 instructions
; decrease the content of (SP) by one without changing
; HL.

	EX	(SP), HL
	DEC	HL
	EX	(SP), HL
	LD	(HLTEMP), HL
	JR	CONT28
;
;***********************************************************
RST30	.ORG	30H

; Instruction RST 30H (opcode F7) is usually used as:
;   i) Software break;
;  ii) Terminator of user's program.
; The effect of this instruction is to save all user's
; registers and return to monitor.

	JR	NMI

;***********************************************************
; This is a part of reset routine.  Address 0028H and
; 0030H are reserved for break point.  Reset routine
; skips this area and resumes here.
;
RESET1	LD	(USERIF), HL	;set user's I register and
							;interrupt flip flop to 0
	JR	RESET2				;monitor resumes at RESET2

;***********************************************************

; The following byte makes the sum of the monitor
; code in ROM zero. ROMTEST is a self-checking routine.
; This routine requires the sum of ROM to be zero.

	.BYTE	ZSUM
;
;***********************************************************
RST38	.ORG	38H

; Entry point of RST 38H (opcode FF) or mode 1 interrupt.
; Fetch the address stored in location FFEEH and FFEFH,
; then jump to this address.  Initially, FFEEH and FFEFH
; are set to 0066H.  So RST 38 will have the same effect
; as software break.  By changing the content of FFEEH
; and FFEFH, the user can define his or her own service
; routine.
; The next three instructions push the contents of FFEEH
; and FFEFH to stack without changing any registers.

	PUSH	HL
	LD	HL, (IM1AD)
	EX	(SP), HL

; The top of the stack is now the address of user
; defined service routine.  Pop out this address then
; branch to it.

	RET
;
;***********************************************************
CONT28:
; This is a part of break service routine.  It continues
; the program at RST28.

	LD	(ATEMP), A

; The monitor has changed the content of user's
; program at break address.  The next 3 instructions
; restore the destroyed content.  BRAD contains the
; break address, BRDA contains the original data at
; break address.

	LD	HL, (BRAD)
	LD	A, (BRDA)
	LD	(HL), A

; Send break enable signal to hardware counter.
; A nonmaskable interrupt will be issued at the 5th M1's

	LD	A, 10000000B
	OUT	(DIGIT), A
	LD	A, (ATEMP)		; 1st M1
	LD	HL, (HLTEMP)	; 2nd M1
	NOP					; 3rd M1
	RET					; 4th M1

; Return to user's program.  Execute the instruction
; at break address.  After finishing one instruction,
; a nonmaskable interrupt happens and control is
; transferred to the monitor again.
;
RESET2:
	LD	HL, USERSTK
	LD	(USERSP), HL	;set user's SP
	XOR	A
	LD	(TEST), A

; TEST is a flag for monitor's own use.  Illegal key-in
; blanking (bit 7 of TEST) and automatic leading zero
; (bit 0) use this flag.  Clear it here.

	LD	IX, HELLO

; Address 0066H is the address for nonmaskable interrupt
; Skip this area, monitor resumes at SETST0

	JP	SETST0

;***********************************************************
NMI	.ORG	66H

; Entry point of nonmaskable interrupt.  NMI will occur
; when MONI key is pressed or when user's program is
; breaked.  The service routine which starts here saves all
; user's registers and status.  It also checks the validity
; of user's SP.

	LD	(ATEMP), A	;save A register
	LD	A, 10010000B
	OUT	(P8255), A	;set 8255 to mode 0.
				;Port A input; B,C output.
	LD	A, 0C0H
	OUT	(DIGIT), A	;disable break and LED's
	LD	A, (ATEMP)	;restore A register
RGSAVE	LD	(HLTEMP), HL	;save register HL
	POP	HL		;get return address from stack
	LD	(ADSAVE), HL	;Save return address into
				;ADSAVE.
	LD	(USERPC), HL	;Set user's PC to return
				;address.
	LD	HL, (HLTEMP)	;restore HL register
	LD	(USERSP), SP	;set user's SP to current SP
	LD	SP, USERIY+2	;save other registers by
	PUSH	IY		;continously pushing them
	PUSH	IX		;onto stack
	EXX
	PUSH	HL
	PUSH	DE
	PUSH	BC
	EXX
	EX	AF,AF'
	PUSH	AF
	EX	AF,AF'
	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF

; The next two instructions save I register.
; The interrupt flip-flop (IFF2) is copied into
; parity flag (P/V) by instruction LD  A, I.
; The interrupt status (enabled or disabled)
; can be determined by testing parity flag.

	LD	A, I
	LD	(USERIF+1), A

; The next four instructions save IFF2 into
; user's IFF.

	LD	A, 0
	JP	PO, SETIF	;PO -- P/V = 0
	LD	A, 1
SETIF	LD	(USERIF), A
;
	LD	SP, SYSSTK	;set SP to system stack

;The next 8 instructions check user's SP.
; If the user's SP points to a location not
; in RAM, display ERR-SP.

	LD	HL, (USERSP)
	LD	IX, ERR_SP
	DEC	HL
	CALL	RAMCHK
	JR	NZ, SETST0
	DEC	HL
	CALL	RAMCHK
	JR	NZ, SETST0

; If the user's stack and system stack are
; overlayed, display SYS-SP.  This checking
; is done by the following instructions.

	LD	IX, SYS_SP
	NOP
	NOP

	LD	DE, -USERSTK+1
	ADD	HL, DE
	JR	C, SETST0
	LD	IX, DISPBF
	SCF			;set carryflag to indicate
				;the user's SP is legal.
	JR	BRRST0
;
SETST0:
; STATE is a memory location containing the monitor status.
; It will be described in detail later.  STATE 0 stands
; for fixed display pattern.  The initial pattern 'HELLO '
; or message 'SYS-SP'... belong to this category.  The next
; two instructions set STATE to zero.

	XOR	A		;set A to 0, also clear Carry flag
	LD	(STATE), A
BRRST0	LD	A, (BRDA)	;restore the data at
				;break address
	LD	HL, (BRAD)
	LD	(HL), A

; If the user's SP is legal (carry set), display user's
; PC and the content at PC. Otherwise, display fixed
; message (ERR-SP or SYS-SP or HELLO ).

	CALL	C, MEMDP2
;
;
;***********************************************************
; Scan the display and keyboard. When a key is detected,
; take proper action  according to the key pressed.

MAIN:
	LD	SP, SYSSTK	;Initialize system stack
	CALL	SCAN		;Scan display and input keys.
				:Routine SCAN will not return until
				;any key is pressed.
	CALL	BEEP		;After a key is detected, there
				;will be accompanied with a beep
				;sound.
	JR	MAIN		;Back to MAIN, get more keys and
				;execute them.
;
;
;***********************************************************
KEYEXEC:

; Input key dispatch routine.
; This routine uses the key code returned by subroutine
; SCAN, which is one byte stored in A register.  The
; range of key codes is from 00 to 1FH.

;  (i) key code = 00 - 0FH :
;      These are hexadecimal keys.  Branch to routine KHEX.

	CP	10H
	JR	C, KHEX

; If the key entered is not hexadecimal, it must be a
; function or subfunction key.  This means the previous
; numeric entry has terminated.  Bit 0 of TEST flag
; must be set at the beginning of a new numeric entry.
; This is done by the next two instructions.  (If bit 0
; of TEST is set, the data buffer will be automatically
; cleared when a hexadecimal key is entered.)

	LD	HL, TEST
	SET	0, (HL)

; (ii) key code = 10H - 17H :
;      (+, -, GO, STEP, DATA, SBR, INS, DEL)
;      There is no state corresponding to these keys.
;      The response of them depends on the current
;      state and minor-state. (E.g., the response of '+'
;      key depends on the current function.  It is illegal
;      when the display is 'HELLO ', but is legal when the
;      display is of 'address-data' form.)  In this
;      documentation, they are named 'sub-function key'.
;      They are all branched by table KSUBFUN and routine
;      BRANCH.

	SUB	10H
	CP	8
	LD	HL, KSUBFUN
	JP	C, BRANCH

;(iii) key code = 18H - 1FH
;      (PC, Addr, CBr, Reg, Move, Rela, WRtape, RDtape)
;      These keys are named 'function key'. They are
;      acceptable at any time.  When they are hit, the
;      monitor will unconditionally enter a new state.
;      STMINOR contains the minor-state, which is required
;      to dispatch some sub-function keys (e.g. +, -).

	LD	IX, DISPBF
	SUB	8
	LD	HL, STATE
	LD	(HL), A		;set STATE to key-code minus 18H
				;The STATE is updated here.  It will
				;be modified later by local service
				;routines if the function-key is PC,
				;Addr or CBr.  For other function-
				;keys, STATE will not be modified
				;later.
	LD	HL, STMINOR
	LD	(HL), 0		;set STMINOR to 0
	LD	HL, KFUN	;KFUN is the base of the branch table
				;the offset is stored in A
	JP	BRANCH

;
;***********************************************************
;STATE:
;   0=FIX	;Display fixed pattern, e.g. 'HELLO '.
;   1=AD	;The hex key entered is interpreted as
		;memory addres.
;   2=DA	;The hex key entered is interpreted as
		;memory data.
;   3=RGFIX	;Display fixed pattern: 'Reg-  ' and
		;expect register name to be entered.
;   4=MV	;Expect parameters for 'Move' function.
;   5=RL	;Expect parameters for 'Rela' function.
;   6=WT	;Expect parameters for 'WRtape' func.
;   7=RT	;Expect parameters for 'RDtape' func.
;   8=RGAD	;Hex-key entered will be interpreted as
		;address name for registers.
;   9=RGDA	;Hex-key entered will be interpreted as
		; data for registers.
;
; Subroutine naming conventions:
;    (i) K???? -- K stands for key, ???? is the key name,
;                 e.g. KINS corresponds to key 'INS'.  Each
;                 time a key ???? is entered, the routine
;                 with name K???? will be executed.  All of
;                 them are branched by table KFUN or KSUBFUN
;   (ii) H???? -- H stands for hexadecimal, ???? is the
;                 current STATE.  For example, routine
;                 HDA will be executed if the entered
;                 key is hexadecimal and STATE is DA now.
;                 These routines are branched by table
;                 HTAB.
;  (iii) I???? -- I stands for increment (+ key), ???? is
;                 the current STATE.  E.g. IMV will be
;                 executed when STATE is MV and '+' key
;                 is entered.  These routines are branched
;                 by table ITAB
;   (iv) D???? -- D stands for decrement (- key), ???? is
;                 the current STATE.  These routines are
;                 branched using table DTAB.
;    (v) G???? -- G stands for 'GO' key, ???? is the current
;                 STATE.  These routines are branched using
;                 table GTAB.

;***********************************************************

; Hexadecimal, '+', '-' and 'GO' keys may be entered after
; different function keys.  The monitor uses branch tables
; and STATE to determine the current function and branch
; to the proper entry point.
;
KHEX:
;Executed when hexadecimal keys are pressed.
;Use HTAB and STATE for further branch.

	LD	C, A		;save A register in C
				;which is the hex key-code.
	LD	HL, HTAB
BR1	LD	A, (STATE)
	JP	BRANCH
;

KINC:
;Branched by KSUBFUN table.
;Executed when '+' key is pressed.
;Use ITAB and STATE for further branch.
;STATE will be stored in A register at BR1.

	LD	HL, ITAB
	JR	BR1

;
KDEC:
;Branched by KSUBFUN table.
;Executed when '-' key is pressed.
;Use DTAB and STATE for further branch.
;STATE will be stored in A register at BR1.

	LD	HL, DTAB
	JR	BR1

;
KGO:
;Branched by KSUBFUN table.
;Executed when 'GO' key is pressed.
;Use DTAB and STATE for further branch.
;STATE will be stored in A register at BR1.

	LD	HL, GTAB
	JR	BR1

;
KSTEP:
;Branched by table KSUBFUN.
;Executed when 'STEP' key is pressed.

	CALL	TESTM		;Check if the left 4 digits
				;of the display are a memory address.
	JP	NZ, IGNORE	;If not, disable all LED's as
				;a warning to the user.  This
				;is done by routine IGNORE.

	LD	A, 10000000B	;This data will be output
				;to port B to enable
				;BREAK.  It is done by
				;routine PREOUT.
	JP	PREOUT

;
KDATA:
;Branched by table KSUBFUN.
;Executed when 'DATA' key is pressed.

	CALL	TESTM		;Check if the left 4 digits
				;of the display are a memory address.
	JR	NZ, TESTRG	;If not, branch to TESTRG
				;to check whether the display
				;is register or not.
	CALL	MEMDP2		;If yes, display the data of
				;that address and set STATE
				;to 2.
	RET
TESTRG	CP	8		;check if the status is 8 or 9
				;(RGAD or RGDA).
	JP	C, IGNORE	;If not, ignore this key and
				;send out a warning message.
	CALL	REGDP9		;If yes, display register and
				;set status to 9 (RGDA).
	RET

;
KSBR:
;Branched by table KSUBFUN.
;Executed when 'SBr' key (set break point)
; is pressed.

	CALL	TESTM		;Check if the display is of
				;'address-data' form.
	JP	NZ, IGNORE	;If not, ignore this key and
				;send out a warning message.
	LD	HL, (ADSAVE)	;If yes, get the address
				;being displayed now.
	CALL	RAMCHK		;Check if this address is
				;in RAM.
	JP	NZ, IGNORE	;If not, ignore the 'SBR' key
				;and send out a warning message.
	LD	(BRAD), HL	;If yes, set this address as
				;a break point.
	CALL	MEMDP2		;Display the data of break
				;address and set STATE to
				;2 (DA).
	RET

;
KINS:
;Branched by table KSUBFUN.
;Executed when 'Ins' key (insert) is pressed.

	CALL	TESTM		;Check if the display is of
				;'addres-data' form.
	JP	NZ, IGNORE	;If not, ignore the 'INS' key
				;and send out a warning message.
	LD	HL, (ADSAVE)	;If yes, get the address being
				;displayed now.

	NOP

	LD	(STEPBF), HL	;Store this address in
				;STEPBF and the next address
				;in STEPBF+4 for later use.
	INC	HL
	LD	(STEPBF+4), HL
	CALL	RAMCHK		;Check if the address to be
				;inserted is in RAM.
	JP	NZ, IGNORE	;If not, ignore the 'INS' key
				;and send out a warning message.
				;If the address to be inserted
				;is in 1800-1DFF, store 1DFE into
				;STEPBF+2
				;Otherwise, ignore the 'INS' key.
				;This is done by the following
				;instructions.
	LD	DE, 1DFEH
	LD	A, H
	CP	1EH
	JR	C, SKIPH1
	CP	20H
	JP	C, IGNORE
	LD	D, 27H
SKIPH1	LD	(STEPBF+2), DE

;When one byte is inserted at some
;address, all data below this address
;will be shifted down one position.
;The last location will be shifted out
;and therefore lost.
;The RAM is divided into 3 blocks as
;insert is concerned.  They are:
;1800-1DFF, 1E00-1FFF and 2000-27FF
;The 2nd block cannot be inserted and
;is usually used as data bank.  System
;data that of course cannot be shifted
;is also stored in this bank.  Each
;block is independent of the other when
;shift is performed, i.e. the data
;shifted out of the first block will not
;be propagated to next block.
;The shift is accomplished by block
;transfer, i.e. MOVE.  This is the
;job of subroutine GMV.
;Routine GMV needs 3 parameters which
;are stored in step-buffer (STEPBF):
;STEPBF: starting address (2 bytes);
;STEPBF+2: ending address (2 bytes);
;STEPBF+4: destination address (2 bytes).

DOMV	CALL	GMV
	XOR	A		;After the RAM has been shifted down,
	LD	(DE), A		;the data of the address to be inserted
				;is cleared to zero.  This is done by
				;these two instructions.  Register
				;DE contains inserted address after GMV
				;is performed.

	LD	HL, (STEPBF+4)	;Store the data in (STEPBF+4)
	LD	(ADSAVE), HL	;into (ADSAVE).
	CALL	MEMDP2		;Display the address and data also
				;set STATE to 2.
	RET
;
KDEL:
;Branched by table KSUBFUN.
;Executed when 'Del' (delete) key is pressed.

	CALL	TESTM		;Check if the display is of
				;'address-data' form.
	JP	NZ, IGNORE	;If not, ignore the 'Del' key and
				;send out a warning message.
				;'Delete' is quite similar to
				;'Insert',except that the memory
				;is shifted up up instead of shifted
				;down.  See the comments on
				;routine KINS for details.
	LD	HL, (ADSAVE)	;Get the address being displayed
				;now.  This is the address to
				;be deleted.


	NOP

	LD	(STEPBF+4), HL
	CALL	RAMCHK		;Check if the address is in RAM.
	JP	NZ, IGNORE	;If not, ignore this key and
				;send out a warning message.
				;Following instructions prepare the
				;parameters for routine GMV in step-
				;buffer.  Refer to routine KINS for
				;detail.
	LD	DE, 1E00H
	LD	A, H
	CP	1EH
	JR	C, SKIPH2
	CP	20H
	JP	C, IGNORE
	LD	D, 28H
SKIPH2	LD	(STEPBF+2), DE
	INC	HL
	LD	(STEPBF), HL
	JR	DOMV
;
;***********************************************************
KPC:
;Branched by table KFUN.
;Executed when 'PC' key is pressed.

	LD	HL, (USERPC)	;Store the user's program
	LD	(ADSAVE), HL	;counter into (ADSAVE)
	CALL	MEMDP2		;Routine MEMDP2 displays the
				;address in (ADSAVE) and its data.
				;It also sets the STATE to 2.
	RET
;
KCBR:
;Branched by table KFUN.
;Executed when 'CBr' (clear break point) key is pressed.

	CALL	CLRBR		;Call subroutine CLRBR to clear
				;break point.  When returned, the HL
				;register will contain FFFF.
	LD	(ADSAVE), HL	;Store FFFF into (ADSAVE)
	CALL	MEMDP2		;Display address and its data.
						;Also set STATE to 2.
	RET
;
KREG:
;Branched by table KFUN.
;Executed when 'Reg' key is pressed.
	LD	IX, REG_	;Routine SCAN uses IX as a pointer
				;for display buffer.  Setting IX to REG
				;will cause SCAN to display 'Reg-  '
	CALL	FCONV		;Decode user's flag F and F' to
				;binary display format.  This
				;format will be used later, when
				;user requires the monitor to
				;display decoded flag by pressing
				;keys 'SZXH', 'XPNC',...
	RET
;
KADDR:
;Branched by table KFUN.
; Executed when 'Addr' key is pressed.

	CALL	MEMDP1		;Display the address stored in
				;(ADSAVE) and its data.
				;Set STATE to 1 (AD).
	RET
;
; Function Move, Relative, Read-tape and
; Write-tape require from one to three
; parameters.  They are stored in STEPBF
; (step buffer).  STMINOR (minor status)
; contains the number of parameters that has
; been entered.  For Move and Relative, the
; default value of the first parameter is
; the address stored in (ADSAVE).  There
; is no default value for the first parameter
; (filename) of Read- and Write-tape.  When the
; function keys are pressed, STMINOR is automatically
; reset to 0.
;

KMV:
;Branched by table KFUN.
;Executed when 'Move' key is pressed.
KRL:
;Branched by table KFUN.
;Executed when 'Rela' (relative) key is pressed.
	LD	HL, (ADSAVE)	;Store the contents of ADSAVE
	LD	(STEPBF), HL	;into STEPBF as default value
				;of first parameter.

KWT:
;Branched by table KFUN.
;Executed when 'WRtape' key is pressed.

KRT:
;Branched by table KFUN.
;Executed when 'RDtape' key is pressed.

	CALL	STEPDP		;Display the parameter that
				;is being entered now by calling
				;subroutine STEPDP.
	RET
;
;***********************************************************
; The following subroutines with name H???
; are the service routines for hexadecimal
; keys corresponding to each STATE.  They
; are all branched by table HTAB and STATE.

HFIX	JP	IGNORE		;When the display is fixed pattern
				;hexadecimal keys are illegal.
				;Disable all LED's as a warning
				;message to the user.  This is what
				;routine IGNORE does.
;
HDA	LD	HL, (ADSAVE)	;Get the address being displayed
				;now from (ADSAVE)
	CALL	RAMCHK		;Check if it is in RAM.
	JP	NZ, IGNORE	;If not, ignore this key and
				;send out a warning message.
	CALL	PRECL1		;If this is the first hexadecimal
				;key entered after function or sub-
				;function key, reset the data of that
				;address to 0. (by routine PERCL1)
	LD	A, C		;The key-code is saved in C by
				;routine KHEX.  Restore it to A.
	RLD			;Rotate the key-code (4 bits) into
				;the address obtained above. (in HL)
	CALL	MEMDP2		;Display the address and data,
				;then set STATE to 2 (DA).
	RET
;
HAD:	LD	HL, ADSAVE
	CALL	PRECL2		;If this is the first hexadecimal
				;key after function key is entered,
				;set the contents of ADSAVE to 0.
	LD	A, C		;The key-code is saved in C
				;by routine KHEX.
				;The next three instructions shift
				;the addres being displayed by
				;one digit.
	RLD
	INC	HL
	RLD
	CALL	MEMDP1		;Display the address and its
				;data.  Also, set STATE to 1.
	RET
;
HRGAD:
HRGFIX:
	LD	A, C
	LD	IX, DISPBF
	LD	HL, STMINOR
	ADD	A, A		;The key-code is the register
				;name.  Double it and store it
	LD	(HL), A		;into STMINOR.

	CALL	REGDP8		;Display register and set
				;STATE to 8. (RGAD)
	RET
;
HRT:
HWT:
HRL:
HMV:	CALL	LOCSTBF		;Use STMINOR and STEPBF
				;to calculate the address
				;of current parameter in
				;step buffer.
	CALL	PRECL2		;If this is the first hex
				;key entered, clear the
				;parameter (2 bytes) by
				;PRECL2.
	LD	A, C		;C contains the key-code
	RLD			;Rotate the parameter (2 bytes)
				;1 digit left with the key-code.

	INC	HL
	RLD
	CALL	STEPDP		;Display the parameter.
	RET
;
HRGDA	CALL	LOCRGBF		;Calculate the address of
				;the register being modified.
	CALL	PRECL1		;If this is the first hex
				;key entered.  Clear the register
				;(1 byte) by PRECL1.
	LD	A, C		;C contains the key-code
	RLD			;Rotate user's register (1 byte)
				;1 digit left with the key-code.

	CALL	REGDP9		;Display the register and set
				;STATE to 9 (RGDA).
	RET
;
;***********************************************************
 ;The following routines with name
 ;I???? are the service routines for
 ;'+' key corresponding to each STATE
 ;They are all branched by table ITAB
 ;and STATE

IFIX:
IRGFIX:
	JP	IGNORE		;'+' key is illegal for state
				;FIX or RGFIX, ignore it.
;
IAD:
IDA:	LD	HL, (ADSAVE)	;Increase the address being
				;displayed now (in ADSAVE)
				;by 1.
	INC	HL
	LD	(ADSAVE), HL
	CALL	MEMDP2		;Display the address and data,
				;then set the STATE to 2 (DA).
	RET
;
IRT:
IWT:
IRL:
IMV:	LD	HL, STMINOR	;STMINOR contains the
				;parameter count, increment
				;it by one.
	INC	(HL)
	CALL	LOCSTNA		;Check if the count is
				;overflowed.
	JR	NZ, ISTEP	;If not overflowed, continue
				;at ISTEP
	DEC	(HL)		;Otherwise, restore the count
				;and ignore the '+' key.
	JP	IGNORE
ISTEP	CALL	STEPDP		;Display the parameter at
				;step buffer.
	RET
;
IRGAD:
IRGDA:	LD	HL, STMINOR	;In these states, the STMINOR
				;contains the register name.
				;Increase it by 1.  If it
				;reaches the last one, reset
				;it to the first one (0).
	INC	(HL)
	LD	A, 1FH
	CP	(HL)
	JR	NC, IRGNA
	LD	(HL), 0
IRGNA	CALL	REGDP9		;Display the register and
				;set STATE to 9
	RET
;
;***********************************************************
 ;The following routines with name
 ;D???? are the service routines for
 ;'-' key corresponding to each state.
 ;They are all branched by table DTAB
 ;and STATE.

DFIX:
DRGFIX:
	JP	IGNORE		;'-' key is illegal for
				;these states.  Ignore it.
;
DAD:
DDA:	LD	HL, (ADSAVE)	;Decrease the address being
				;displayed now (in ADSAVE)
				;by one.
	DEC	HL
	LD	(ADSAVE), HL
	CALL	MEMDP2		;Display the address and data,
				;set STATE to 2 (DA).
	RET
;
DRT:
DWT:
DRL:
DMV:	LD	HL, STMINOR	;In these states, STMINOR contains
				;the parameter count.  Decrease it by one.
				;If overflow occurs, restore STMINOR and
				;ignore the '-' key.  Otherwise continue
				;at DSTEP.

	DEC	(HL)
	CALL	LOCSTNA
	JR	NZ, DSTEP
	INC	(HL)
	JP	IGNORE
DSTEP	CALL	STEPDP		;Display the parameter.
	RET
;
DRGAD:
DRGDA:	LD	HL, STMINOR	;In these states, STMINOR
				;contains the register name.
				;Decrease it by one.  If it
				;goes below zero,  set it to
				;the highest value (1F).
	DEC	(HL)
	LD	A, 01FH
	CP	(HL)
	JR	NC, DRGNA
	LD	(HL), 1FH
DRGNA	CALL	REGDP9		;Display the register and
				;set STATE to 9.
	RET
;
;***********************************************************
 ;The following routines with name
 ;G???? are the service routines for
 ;'GO' key corresponding to each
 ;state.  They are all branced by
 ;table GTAB and STATE.

GFIX:
GRGFIX:
GRGAD:
GRGDA:	JP	IGNORE		;'GO' key is illegal for
				;these states.  Ignore it.
;
GAD:
GDA:	LD	HL, (BRAD)	;Get the address of break
				;point.
	LD	(HL), 0EFH	;Instruction RST28H.
				;The content of break address
				;is changed to RST 28H before
				;the control is transfered to
				;user's program.  This
				;will cause a trap when user's
				;PC passes this point.
	LD	A, 0FFH		;Save FF into TEMP.  This data
				;will be output to port C later.
				;FF is used to disable break point.
PREOUT	LD	(TEMP), A	;Store A into TEMP.
	LD	A, (USERIF)	;Save two instructions into
				;TEMP and TEMP+1.  THese two
				;instructions will be executed
				;later.  If the user's IFF
				;(interrupt flip-flop) is 1,
				;the instructions are 'EI RET'.
				;Otherwise, they are 'DI RET'.
	BIT	0, A
	LD	HL, 0C9FBH	;'EI', 'RET'
	JR	NZ, EIDI
	LD	L, 0F3H		;'DI'
EIDI	LD	(TEMP+1), HL
	LD	SP, REGBF	;Restore user's registers by
				;setting SP to REGBF (register
				;buffer) and continuously popping
				;the stack.
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	EX	AF, AF'
	POP	AF
	EX	AF, AF'
	EXX
	POP	BC
	POP	DE
	POP	HL
	EXX
	POP	IX
	POP	IY
	LD	SP, (USERSP)	;Restore user's SP.
	LD	(USERAF+1), A	;Temporarily save A
	LD	A, (USERIF+1)	;Restore user's I
	LD	I, A
				;The next 3 instructions
	PUSH	HL		;push the address being
				;displayed now (in ADSAVE)
				;onto stack without changing
				;HL register.  This address will be
				;treated as user's new PC.
	LD	HL, (ADSAVE)
	EX	(SP), HL
	LD	A, (TEMP)	;Output the data stored in
				;TEMP to port C of 8255
				;This data is prepared by
				;routine KSTEP or GAD or
				;GDA.  In first case, it is
				;10111111 and will enable
				;break point.  In other
				;cases, it is FF and will
				;disable break point.
				;If break is enabled, non-
				;maskable interrupt will occur
				;5 M1's after the OUT instruction.
	OUT	(DIGIT), A
	LD	A, (USERAF+1)	;1st M1,
				;Restore A register.
	JP	TEMP+1		;2nd M1,
				;Execute the two instructions
				;stored in RAM.  They are:
				;    EI (or DI)     ;3rd M1
				;    RET            ;4th M1
				;The starting address of user's
				;program has been pushed onto
				;the top of the stack.  RET pops
				;out this address and transfers
				;control to it.  The first M1
				;of user's program will be the
				;5th M1 after OUT. If break point
				;is enabled, NMI will occur after
				;this instruction is completed.
				;This is the mechanism of single
				;step.
;
;***********************************************************
GMV	LD	HL, STEPBF
	CALL	GETP		;Load parameters from
				;step buffer into registers.
				;Also check if the parameters
				;are legal.  After GETP,
				;HL = start address of source
				;BC = length to MOVE.
	JR	C, ERROR	;Jump to ERROR if the
				;parameters are illegal. (I.e., Ending
				;address < starting address.)
	LD	DE, (STEPBF+4)	;Load destination
				;address into DE.
	SBC	HL, DE		;Compare HL and DE to
				;determine move up or down.
	JR	NC, MVUP
				;Move down:
	EX	DE, HL		;HL = destination address
	ADD	HL, BC		;HL = dest. address + length
	DEC	HL		;HL = end address of dest.
	EX	DE, HL		;DE = end address of dest.
	LD	HL, (STEPBF+2)	;HL = end address of source
	LDDR			;block transfer instruction
	INC	DE		;DE = last address moved
	JR	ENDFUN		;Continue at ENDFUN
MVUP:				;Move up:
	ADD	HL, DE		;HL is destroyed by
				;SBC HL, DE.  Restore HL.
	LDIR			;block transfer
	DEC	DE		;DE = last address moved
	JR	ENDFUN		;Continue at ENDFUN
;
;***********************************************************
GRL	LD	DE, (STEPBF)	;Load starting address
				;into DE.
	INC	DE		;Increase this address by 2.
				;Relative address is used in
				;instruction JR or DJNZ.
				;The codes for them are 2 bytes.
				;The PC is increased by 2 after
				;opcode is fetched.
	INC	DE
	LD	HL, (STEPBF+2)	;Load destination
				;address into HL.
	OR	A
	SBC	HL, DE		;Calculate difference.
	LD	A, L		;Check if the offset is between
				;+127 (007FH) and -128 (FF80H).
				;If the offset is positive, both H
				;and bit 7 of L must be zero; if it
				;is negative, H and bit 7 of L must
				;be FF and 1.  In both cases, adding
				;H with bit 7 of L results in 0.
	RLA			;Rotate bit 7 of L into carry flag.
	LD	A, H
	ADC	A, 0		;ADD H and bit 7 of L.
	JR	NZ, ERROR	;Branch to ERROR if
				;the results is nonzero.
	LD	A, L
	DEC	DE
	LD	(DE), A		;Save the offset into
				;the next byte of opcode.
				;(DJNZ or JR)
;
ENDFUN:
	LD	(ADSAVE), DE	;Save DE into ADSAVE.
	CALL	MEMDP2		;Display this address and
				;its data.  Set STATE to 2.
	RET
;
;***********************************************************
GWT:
	CALL	SUM1		;Load parameters from
				;step buffer into registers.
				;Check if the parameters
				;are legal.  If legal, calculate
				;the sum of all data to be output
				;to tape.
	JR	C, ERROR	;Branch to ERROR if the
				;parameters are illegal. (length is
				;negative)
	LD	(STEPBF+6), A	;Store the checksum into
				;STPEGBF+6.
	LD	HL, 4000	;Output 1 kHz square
				;wave for 4000 cycles.
				;Leading sync. signal.
	CALL	TONE1K
	LD	HL, STEPBF	;Output 7 bytes starting
				;at STEPBF. (Include:
				;filename, starting, ending
				;address and checksum)
	LD	BC, 7
	CALL	TAPEOUT
	LD	HL, 4000	;Output 2 kHz square
				;wave for 4000 cycles.
				;Middle sync.  The file name of the
				;file being read will be displayed
				;in this interval.
	CALL	TONE2K
	CALL	GETPTR		;Load parameters into
				;registers. (Starting, ending and
				;length).
	CALL	TAPEOUT		;Output user's data.
	LD	HL, 4000	;Output 4000 cycles of
				;2 kHz square wave.
				;(Tail sync.)
	CALL	TONE2K
ENDTAPE	LD	DE, (STEPBF+4)	;DE = last address
	JR	ENDFUN		;Continue at ENDFUN
;
ERROR	LD	IX, ERR_	;IX points to '-Err  '
	JP	SETST0		;Set STATE to 0 by
				;branching to SETST0.
;
;***********************************************************
GRT:
	LD	HL, (STEPBF)	;Temporarily save filename.
	LD	(TEMP), HL
LEAD	LD	A, 01000000B	;decimal point
	OUT	(SEG7), A	;When searching for filename,
				;the display is blank initially.
				;If the data read from MIC is
				;acceptable 0 or 1, the display
				;becomes '......'.
	LD	HL, 1000
LEAD1	CALL	PERIOD		;The return of PERIOD
				;is in flag:
				;  NC -- tape input is 1 kHz;
				;   C -- otherwise.
	JR	C, LEAD		;Loop until leading sync.
				;is detected.
	DEC	HL		;Decrease HL by one when
				;one period is detected.
	LD	A, H
	OR	L		;Check if both H and L are 0.
	JR	NZ, LEAD1	;Wait for 1000 periods.
				;The leading sync. is accepted
				;if it is longer than 1000
				;cycles (1 second).
LEAD2	CALL	PERIOD
	JR	NC, LEAD2	;Wait all leading sync. to
				;pass over.
;
	LD	HL, STEPBF	;Load 7 bytes from
				;tape into STEPBF.
	LD	BC, 7
	CALL	TAPEIN
	JR	C, LEAD		;Jump to LEAD if input
				;is not succesful.
	LD	DE, (STEPBF)	;Get filename from
				;step buffer.
	CALL	ADDRDP		;Convert it to display
				;format.
	LD	B, 150		;Display it for 1.5 sec.
FILEDP	CALL	SCAN1
	DJNZ	FILEDP
	LD	HL, (TEMP)	;Check if the input
				;filename equals to the
				;specified filename.
	OR	A
	SBC	HL, DE
	JR	NZ, LEAD	;If not, find the leading
				;sync. of next file.

				;If filename is found
	LD	A, 00000010B	;segment '-'
	OUT	(SEG7), A	;Display '------'.
	CALL	GETPTR		;The parameters (starting,
				;ending address and check-
				;sum) have been loaded into
				;STEPBF.  Load them into
				;registers, calculate the block
				;length and check if they are
				;legal.
	JR	C, ERROR	;Jump to ERROR if the
				;parameters are illegal.
	CALL	TAPEIN		;Input user's data.
	JR	C, ERROR	;Jump to ERROR if input
				;is not succesful.
	CALL	SUM1		;Calculate the sum of all
				;input data.
	LD	HL, STEPBF+6
	CP	(HL)		;Compare it with the
				;checksum calculated and stored by
				;'WRtape'.
	JR	NZ, ERROR	;Jump to ERROR if not
				;matched.
	JR	ENDTAPE		;Continue at ENDTAPE.
;
;***********************************************************
BRANCH:
;Branch table format:
;   byte 1,2 : address of the 1st routine in
;              each group.
;   byte 3   : difference between the address
;              of 1st and 1st routine, which is
;              of course 0.
;   byte 4   : difference between the address
;              of 2nd and 1st routine
;   byte 5   : difference between the address
;              of 3rd and 1st routine
;    ...
;    ...
;    ...
; HL : address of branch table
; A  : the routine number in its group
; Such branch tables can save table length and avoid page
; (256 bytes) boundary problems.

	LD	E, (HL)		;Load the address of 1st
				;routine in the group into
				;DE register.
	INC	HL
	LD	D, (HL)
	INC	HL		;Locate the pointer of difference
				;table.
	ADD	A, L
	LD	L, A
	LD	L, (HL)		;Load the address
				;difference into L.
	LD	H, 0
	ADD	HL, DE		;Get routine's real address
	JP	(HL)		;Jump to it.
;
;***********************************************************
IGNORE:
	LD	HL, TEST
	SET	7, (HL)		;Routine SCAN will check bit
				;7 of TEST.  If it is set,
				;all LEDs will be disabled.
				;This is a warning message to
				;the user when a illegal key
				;is entered.
	RET
;
;***********************************************************
INI:
; Power-up initialization.
	LD	IX, BLANK	;BLANK is the initial display
				;pattern.
				;Display the following
				;patterns sequence, each 0.16
				;seconds:
				;    '      '
				;    '     H'
				;    '    HE'
				;    '   HEL'
				;    '  HELL'
				;    ' HELLO'
				;    'HELLO '

	LD	C, 7		;pattern count
INI1	LD	B, 16		;Display 0.16 second.
INI2	CALL	SCAN1
	DJNZ	INI2
	DEC	IX		;next pattern
	DEC	C
	JR	NZ, INI1
;
	LD	A, PWCODE
	JP	INI3
INI4	LD	HL, NMI
	LD	(IM1AD), HL	;Set the service routine
				;of RST 38H to NMI, which is the
				;nonmaskable interrupt service
				;routine for break point and
				;single step.
CLRBR:
; Clear break point by setting
; the break point address to
; FFFF.  This is a non-existant
; address, so break can never
; happen.

	LD	HL, 0FFFFH
	LD	(BRAD), HL
	RET
;
TESTM:
; Check if the display is of 'addres-data'
; form, i.e. STATE 1 or 2.
; The result is stored in zero flag.
;   Z:	yes
;  NZ:	no

	LD	A, (STATE)
	CP	1
	RET	Z
	CP	2
	RET
;
PRECL1:
; Pre-clear 1 byte.
; If bit 0 of TEST is not 0, load 0 into (HL).
; Bit 0 of TEST is cleared after check.
; Only AF register is destroyed.

	LD	A, (TEST)
	OR	A		;Is bit 0 of TEST zero?
	RET	Z
	LD	A, 0
	LD	(HL), A		;Clear (HL)
	LD	(TEST), A	;Clear TEST too.
	RET
;
PRECL2:
; Pre-clear 2 bytes.
; If bit 0 of TEST is nonzero, clear (HL)
; and (HL+1).
; Only AF register is destroyed.

	CALL	PRECL1
	RET	Z
	INC	HL
	LD	(HL), A
	DEC	HL
	RET
;
;***********************************************************
; Memory display format: (address-data)

;      i) A.A.A.A. D D -- State is AD.  Four decimal points
;                         under the address field indicate
;                         that the numeric key entered will
;                         be interpreted as memory address.
;     ii) A A A A  D.D.-- State is DA.  Two decimal points
;                         under the data field indicate
;                         the monitor is expecting user to
;                         enter memory data.
;    iii) A.A.A.A. D.D.-- Six decimal points indicate the
;                         address being displayed is set
;                         as a break point.

MEMDP1:
	LD	A, 1		;Next STATE = 1
	LD	B, 4		;4 decimal points active
	LD	HL, DISPBF+2	;The first active decimal
				;point is in DISPBF+2, the
				;last in DISPBF+5.
	JR	SAV12		;Continue at SAV12.
MEMDP2:
	LD	A, 2		;Next STATE = 2
	LD	B, 2		;2 active decimal points
	LD	HL, DISPBF	;1st decimal point is in
				;DISPBF, 2nd in DISPBF+1.
SAV12	LD	(STATE), A	;Update STATE
	EXX			;Save register HL, BC, DE
	LD	DE, (ADSAVE)	;The address to be
				;displayed is stored in
				;(ADSAVE).  Load it into
				;DE register.
	CALL	ADDRDP		;Convert this address to
				;display format and store it
				;into DISPBF+2 - DISPBF+5.
	LD	A, (DE)		;Load the data of this
				;address into A register
	CALL	DATADP		;Convert this data to
				;display format and store it
				;into DISPBF - DISPBF+1
BRTEST:
; The next 3 instructions serve to refresh the
; data at break address every time memory is
; displayed.
	LD	HL, (BRAD)	;Get break point address.
	LD	A, (HL)		;Get the data of this
				;address into A register.
	LD	(BRDA), A	;Store it into BRDA (break data).
	OR	A
	SBC	HL, DE		;Check if the address to
				;be displayed is break point.
	JR	NZ, SETPT1	;If not, jump to SETPT1.
	LD	B, 6		;6 active decimal points.
	LD	HL, DISPBF	;1st decimal point is in
				;DISPBF; 6th in DISPBF+5.
	EXX
SETPT1	EXX
SETPT	SET	6,(HL)		;Set decimal points.
				;Count in B, first address
				;in HL register.
	INC	HL
	DJNZ	SETPT
	RET
;
;***********************************************************
; Step display format: (this format is used when user is
; entering parameters for Move, Rela, WRtape, RDtape.)

;         P.P.P.P. - N

; 'P' is the digit of parameter.  Four decimal points
; indicate P's are being modified now.  N is the mnemonic of
; the parameter:
;      i) Move   S -- starting address
;                E -- ending address
;                D -- destination address
;     ii) Rela   S -- source address
;                D -- destination address
;    iii) WRtape F -- file name
;                S -- starting address
;                E -- ending address
;     iv) Rdtape F -- filename

STEPDP:
;Display step buffer and its parameter name.
;Input:	STATE
;	STMINOR (parameter count)
;registers destroyed: AF, BC, DE, HL

	CALL	LOCSTBF		;Get parameter address
	LD	E, (HL)		;Load parameter into DE
	INC	HL
	LD	D, (HL)
	CALL	ADDRDP		;Convert this paramter to
				;display format (4 digits)
				;and store it into DISPBF+2
				; - DISPBF+5
	LD	HL, DISPBF+2	;Set 4 decimal points
				;From DISPBF+2 to DISPBF+5
	LD	B, 4
	CALL	SETPT
	CALL	LOCSTNA		;Get parameter name.
	LD	L, A
	LD	H, 2		;Pattern '-' for 2nd rightmost
				;digit.
	LD	(DISPBF), HL
	RET
;
LOCSTBF:
;Get the location of paramter.
; address = STEPBF + STMINOR*2
;registers destroyed: AF, HL

	LD	A, (STMINOR)	;Get parameter count.
	ADD	A, A		;Each parameter has 2 bytes
	LD	HL, STEPBF	;Get base address
	ADD	A, L
	LD	L, A
	RET
;
LOCSTNA
 ;Get parameter name.
 ;Input: STATE, STMINOR
 ;Output: parameter name in A, and Z flag.

 ;registers destroyed: AF, DE
	LD	A, (STATE)	;Get STATE.
				;Possible states are:
				;4,5,6,7. (Move, Rel,
				;WRtape, RDtape)
	SUB	4		;Change 4,5,6,7 to
				;0,1,2,3
	ADD	A, A		;Each state has 4 bytes for names
	ADD	A, A
	LD	DE, STEPTAB
	ADD	A, E
	LD	E, A		;Now, DE contains the
				;address of 1st name
				;for each state.
	LD	A, (STMINOR)	;Get parameter count
	ADD	A, E		;DE <--- DE + A
	LD	E, A
	LD	A, (DE)		;Get parameter name.
	OR	A		;Change zero flag.  If the
				;returned pattern (in A) is
				;zero,  the '+' or '-' must
				;have been pressed beyond legal
				;parameter boundary. (Check if
				;parameter name got from STEPTAB
				;is zero)
	RET
;
;***********************************************************
; Register display format:

;       i)  X X X X  Y Y -- State is REGAD.  The numeric
;                           data entered is interpreted as 
;                           register name.
;                           YY is the register name, the
;                           data of that register pair is,
;                           XXXX.

;      ii)  X X X.X. Y Y or
;     iii)  X.X.X X  Y Y -- State is REGDA.  The unit of
;                           register modification is byte.
;                           The numeric data enterd will
;                           change the byte with decimal
;                           points under it.  Decimal points
;                           can be moved by '+' and '-' keys

REGDP8:
; Display register and set STATE to 8.

	LD	A, 8		;Next state = 8
	JR	RGSTIN

REGDP9:
; Display register and set STATE to 9

	LD	A, 9		;Next state = 9

RGSTIN:
; Update STATE by register A.
; Display user's  register (count
; contained in STMINOR).
; registers destroyed: AF, BC, DE, HL

	LD	(STATE), A	;Update STATE
	LD	A, (STMINOR)	;Get register coutn.
	RES	0, A		;Registers are displayed by
				;pairs.  Find the count
				;of pair leader.  (count of
				;the lower one)
	LD	B, A		;Temprorarily save A.
	CALL	RGNADP		;Find register count.
				;Store them into DISPBF
				;and DISPBF+1.
	LD	A, B		;Restore A (register pair leader).
	CALL	LOCRG		;Get the address of
				;user's register.
	LD	E, (HL)		;Get register data. (2 bytes)
	INC	HL
	LD	D, (HL)
	LD	(ADSAVE), DE	;Convert them to display
				;format and store into
				;display buffer.
	CALL	ADDRDP
	LD	A, (STATE)
	CP	9		;If STATE equals to 9 (RGDA),
				;set 2 decimal points.
				;Otherwise return here.
	RET	NZ
	LD	HL, DISPBF+2
	LD	A, (STMINOR)	;Get register name.
	BIT	0, A		;If this register is
				;group leader, set decimal
				;points of two central digits.
				;Otherwise set two left digits.
	JR	Z, LOCPT
	INC	HL
	INC	HL
LOCPT	SET	6, (HL)		;Set decimapl points of
				;(HL) and (HL+1)
	INC	HL
	SET	6, (HL)
	CALL	FCONV		;Convert user's flag (F, F')
				;to binary display format.
	RET
;
RGNADP:
; Get the patterns of register names and
; store them into DISPBF and DISPBF+1.
; Input: A contains register count of
;        pair leader.
; registers destroyed: AF,DE,HL

	LD	HL, RGTAB	;Get address of pattern
				;table.
	ADD	A, L
	LD	L, A
	LD	E, (HL)		;Get first pattern
	INC	HL
	LD	D, (HL)		;Get 2nd pattern.
	LD	(DISPBF), DE
	RET
;
LOCRGBF:
; Get the address of user's register.
; Register name contained is STMINOR.
; Destroys HL, AF.

	LD	A, (STMINOR)
LOCRG	LD	HL, REGBF
	ADD	A, L
	LD	L, A
	RET
;
FCONV:
; Encode or decode user's flag register.
; STMINOR contains the name of the flag
; being displayed now.
; registers destroyed: AF, BC, HL.

	LD	A, (STMINOR)	;Get register name.
	OR	A		;Clear carry flag.
	RRA			;name of I register: 17H,
				;name of IFF: 16H.
				;Rotate right one bit, both
				;become 0BH.
	CP	0BH
	JR	Z, FLAGX	;Jump to FLAGX if
				;I or IFF is being
				;displayed now.
	LD	C, A		;Otherwise, mask out bit
				;1 to bit 7 fof user's IFF.
				;IFF is only 1 bit, monitor
				;uses one byte to store it,
				;masking out bit 1-7 is to
				;ignore the useless bits.
				;This is done only when the
				;user is not modifying IFF.
				;If user is modifying IFF,
				;monitor will display whatever
				;he enters, even if bit 1-7
				;are not all zero.
				;A register is not changed
				;after doing this.
	LD	HL, USERIF
	LD	A, (HL)
	AND	00000001B
	LD	(HL), A
	LD	A, C
FLAGX	CP	0CH		;If STMINOR contains
				;the name of SZXH, XPNC,
				;SZXH' or XPNC', after
				;rotating right one bit
				;it will be greater than
				;or equal to 0CH.
				;Decode user's flag if it
				;is not being modified now,
				;encode it otherwise.
	JR	NC, FCONV2
FCONV1	LD	A, (USERAF)	;Get user's F register
	CALL	DECODE		;Decode upper 4 bits.
	LD	(FLAGH), HL
	CALL	DECODE		;Decode lower 4 bits.
	LD	(FLAGL), HL
	LD	A, (UAFP)	;Get user's F' register.
	CALL	DECODE
	LD	(FLAGHP), HL
	CALL	DECODE
	LD	(FLAGLP), HL
	RET
FCONV2	LD	HL, (FLAGH)	;Get the binary form
				;of 4 upper bits of
				;user's F register.
	CALL	ENCODE		;Encode it.
	LD	HL, (FLAGL)	;Encode 4 lower bits.
	CALL	ENCODE
	LD	(USERAF), A	;Save the encoded
				;result into USERAF.
	LD	HL, (FLAGHP)	;Encode F' register.
	CALL	ENCODE
	LD	HL, (FLAGLP)
	CALL	ENCODE
	LD	(UAFP), A
	RET
;
DECODE:
; Decode bit 7-4 of A register.
; Each bit is extended to 4 bits.
; 0 becomse 0000, 1 becomse 0001.
; The output is stored in HL, which
; is 16 bits in length.  Also, after
; execution, bits 7-4 of A register are
; bits 3-0 of A before execution.
; Registers destroyed: AF, B, HL

	LD	B, 4		;Loop 4 times
DRL4	ADD	HL, HL		;Clear rightmost 3
				;bits of HL.
	ADD	HL, HL
	ADD	HL, HL
	RLCA
	ADC	HL, HL		;The 4th bit of HL
				;is determined by carry
				;flag, which is the MSB
				;of A register.
	DJNZ	DRL4
	RET
;
ENCODE:
; Encode HL register.  Each 4 bits of HL
; are encoded to 1 bit. 0000 become 0,
; 0001 become 1.  The result is stored
; in bit 3-0 of A register.  Also, after
; execution, bit 7-4 of A are bit 3-0
; before execution.
; Registers AF, B, HL are destroyed.

	LD	B, 4		;Loop 4 times.
ERL4	ADD	HL, HL		;Shift HL left 4 bits.
				;Bit 12 of HL will be
				;shifted into carry flag.
	ADD	HL, HL
	ADD	HL, HL
	ADD	HL, HL
	RLA			;Rotate carry flag into
				;A register.
	DJNZ	ERL4
	RET
;
;***********************************************************
SUM1:
; Calculate the sum of the data in a memory
; block.  The starting and ending address
; of this block are stored in STEPBF+2 - STEPBF+4
;   Registers AF, BC, DE, HL are destroyed.

	CALL	GETPTR		;Get parameters from
				;step buffer.
	RET	C		;Return if the parameters
				;are illegal.
SUM:
; Calculate the sum of a memory block.
; HL contains the starting address of
; this block, BC contains the length.
; The result is stored in A.  Registers
; AF, BC, HL are destroyed.

	XOR	A		;Clear A
SUMCAL	ADD	A, (HL)		;Add
	CPI
	JP	PE, SUMCAL
	OR	A		;Clear flags.
	RET
;
GETPTR:
; Get parameters from step buffer.
; Input:  (STEPBF+2) and (STEPBF+3) contain
;         starting address.
;         (STEPBF+4) and (STEPBF+5) contain
;         ending address.
; Output: HL register contains the starting
;         address.
;         BC register contains the length.
;         Carry flag 0 -- BC positive
;                    1 -- BC negative
; Destroyed registers: AF, BC, DE, HL.

	LD	HL, STEPBF+2
GETP	LD	E, (HL)		;Load starting address
				;into DE.
	INC	HL
	LD	D, (HL)
	INC	HL
	LD	C, (HL)
	INC	HL		;Load ending address
				;into HL.
	LD	H, (HL)
	LD	L, C
	OR	A		;Clear carry flag.
	SBC	HL, DE		;Find difference.
				;Carry flag is changed here.
	LD	C, L
	LD	B, H
	INC 	BC		;Now BC contains the
				;length.
	EX	DE, HL		;Now HL contains the
				;starting address.
	RET
;
TAPEIN:
; Load a memory block from tape.
; Input: HL -- starting address of the block
;        BC -- length of the block
; Output: Carry flag 1 -- reading error
;                    0 -- no error
; Destroyed registers: AF, BC, DE, HL, AF', BC', DE', HL'

	XOR	A		;Clear carry flag.
				;At beginning, the reading is
				;no error.
	EX	AF, AF'
TLOOP	CALL	GETBYTE		;Read 1 byte from tape.
	LD	(HL), E		;Store it into memory.
	CPI
	JP	PE, TLOOP	;Loop until length
				;is zero.
	EX	AF, AF'
	RET
;
GETBYTE:
; Read one byte from tape.
; Output: E -- data read
;         Carry of F' 1 -- reading error
;                     0 -- no error
; Destroy registers: AF, DE, AF', BC', DE', HL',
;Byte format:

; start bit bit bit bit bit bit bit bit stop
;  bit   0   1   2   3   4   5   6   7   bit

	CALL	GETBIT		;Get start bit.
	LD	D, 8		;Loop 8 times
BLOOP	CALL	GETBIT		;Get one data bit.
				;Result in carry flag.
	RR	E		;Rotate it into E.
	DEC	D
	JR	NZ, BLOOP
	CALL	GETBIT		;Get stop bit.
	RET
;
;
GETBIT:
; Read one bit from tape.
; Output: Carry of F:  0 -- this bit is 0
;                      1 -- this bit is 1
;         Carry of F': 1 -- reading error
;                      0 -- no error
; Destroyed registers: AF, AF', BC', DE', HL'
; Bit format:

;   0 -- 2 kHz 8 cycles + 1kHz 2 cycles.
;   1 -- 2 kHz 4 cycles + 1kHz 4 cycles.

	EXX			;Save HL, BC, DE registers

; The tape-bit format of both 0 and 1 are
; the same form: a high freq part followed by a
; low freq part. The difference between 0 and 1
; is the number of high freq cycles and low freq
; cycles.  Thus, a high freq period may have
; two meanings:
;  i) It is used to count the number of high
;     freq cycles of the current tape-bit;
; ii) If a high freq period is detected
;     immediately after a low freq period, then
;     this period is the first cycle of next
;     tape-bit and is used as a terminator of the
;     last tape-bit.

; Bit 0 of H register is used to indicate the usage
; of a high freq period.  If this bit is zero, high
; freq period causes counter increment for the current
; tape-bit.  If the high freq part has passed, bit 0
; of H is set and the next high freq period will be used
; as a terminator.
; L register is used to up/down count the number of periods.
; when a high freq period is read, L is increased by
; 1; when a low freq period is read, L is decreased
; by 2. (The time duration for each count is 0.5 ms.)
; At the end of a tape-bit, positive and negative L
; stand for 0 and 1 respectively.

	LD	HL, 0		;Clear bit 0 of H,
				;Set L to 0.
COUNT	CALL	PERIOD		;Read one period.
	INC	D		;The next 2 instructions
				;check if D is zero.  Carry
				;flag is not affected.
	DEC	D
	JR	NZ, TERR	;If D is not zero, jump
				;to error routine TERR.
				;(Because the period is too
				;much longer than that of 1 kHz.)
	JR	C, SHORTP	;If the period is short
				;(2 kHz), jump to SHORTP.
	DEC	L		;The period is 1 kHz,
				;decrease L by 2.  And set
				;bit 0 of H to indicate this
				;tape-bit has passed high freq
				;part and reaches its low freq part.
	DEC	L
	SET	0, H
	JR	COUNT
SHORTP	INC	L		;The period is 2 kHz,
				;increase L by 1.
	BIT	0, H		;If the tape-bit has passed
				;its high freq part, high frequency
				;means this bit is all over and
				;next bit has started.
	JR	Z, COUNT
				;L = (# of 2kHz periods) - 
				;    2*(# of 1kHz periods)
	RL	L
				; 0 --- NCarry (L positive)
				; 1 ---  Carry (L negative)
				;The positive or negative sign of
				;L corresponds to the tape-bit data.
				;'RL  L' will shift the sign bit of
				;L into carry flag.  After this
				;instruction, the carry flag
				;contains the tape-bit.
	EXX			;Restore BC', DE', HL'
	RET
TERR	EX	AF, AF'
	SCF			;Set carry flag of F' to indicate error.
	EX	AF, AF'
	EXX
	RET
;
PERIOD:
; Wait the tape to pass one period.
; The time duration is stored in DE.  The
; unit is loop count.  Typical value for
; 2kHz is 28, for 1 kHz is 56.
; Use (56+28)/2 as threshold.  The returned
; result is in carry flag. (1 kHz -- NC, 2 kHz -- C)
; Registers destroyed: AF, DE

	LD	DE, 0
LOOPH	IN	A, (KIN)	;Bit 7 of port A is Tapein.
	INC	DE
	RLA
	JR	C, LOOPH	;Loop until input goes low.
	LD	A, 11111111B	;Echo the tape input to
				;speaker.
	OUT	(DIGIT), A
LOOPL	IN	A, (KIN)
	INC	DE
	RLA
	JR	NC, LOOPL	;Loop until input goes high.
	LD	A, 01111111B	;Echo the tape input to
				;speaker.
	OUT	(DIGIT), A
	LD	A, E		;Compare the result with
				;the threshold.
	CP	MPERIOD
	RET
;
;***********************************************************
TAPEOUT:
; Output a memory block to tape.
; Input: HL -- starting address of the block
;        BC -- length of the block
; Destroyed registers: AF, BC, DE, HL, BC', DE', HL'

	LD	E, (HL)		;Get the data.
	CALL	OUTBYTE		;Output to tape.
	CPI
	JP	PE, TAPEOUT	; Loop until finished.
	RET
;
OUTBYTE:
; Output one byte to tape.  For tape-byte
; format, see coments on GETBYTE.
; Input: E -- data
; Destroyed registers: AF, DE, BC', DE', HL'

	LD	D, 8		;Loop 8 times
	OR	A		;Clear carry flag.
	CALL	OUTBIT		;Output start bit.
OLOOP	RR	E		;Rotate data into carry
	CALL	OUTBIT		;Output the carry
	DEC	D
	JR	NZ, OLOOP
	SCF			;Set carry flag.
	CALL	OUTBIT		;Output stop bit
	RET
;
OUTBIT:
; Output one bit to tape.
; Input: data in carry flag.
; Destroyed registers: AF, BC', DE', HL'
	EXX			;Save BC, DE, HL.
	LD	H, 0
	JR	C, OUT1		;If data=1, output 1.
OUT0:				;2 kHZ 8 cycles, 1kHz 2 cycles.
	LD	L, ZERO_2K
	CALL	TONE2K
	LD	L, ZERO_1K
	JR	BITEND
;
OUT1:				;2 Khz 4 cycles, 1kHz 4 cycles.
	LD	L, ONE_2K
	CALL	TONE2K
	LD	L, ONE_1K
BITEND	CALL	TONE1K
	EXX			;Restore registers.
	RET
;
;***********************************************************
;
;        UTILITY SUBROUTINES
;
;***********************************************************
;
; Function: Generate square wave to the MIC & speaker
;           on 'HELLO '
; Input:    C -- period = 2*(44+13*C) clock states.
;          HL -- number of periods.
; Output:  none.
; Destroyed registers: AF, B, DE, HL.
; Call: none

TONE1K:
	LD	C, F1KHZ
	JR	TONE
TONE2K:
	LD	C, F2KHZ
TONE:				;Half period: 44+13*C states
	ADD	HL, HL		;Double for half-cycle count
	LD	DE, 1
	LD	A, 0FFH
SQWAVE	OUT	(DIGIT), A	;Bit-7 tapeout
	LD	B, C
	DJNZ	$		;Half period delay
	XOR	80H		;Toggle output
	SBC	HL, DE		;Decrement one count
	JR	NZ, SQWAVE
	RET
;
;***********************************************************
; Function: check if a memory address is in RAM.
; Input: HL -- address to be checked.
; Output: Zero flag -- 0, ROM or nonexistant;
;                      1, RAM.
; Destroyed registers: AF
; Call: none

RAMCHK:
	LD	A, (HL)
	CPL
	LD	(HL), A
	LD	A, (HL)
	CPL
	LD	(HL), A
	CP	(HL)
	RET
;
;***********************************************************
; Function: Scan the keyboard and display.  Loop until
;           a key is detected.  If the key is already
;           pressed when this routien starts execution,
;           return when next key is entered.
; Input:    IX points to the buffer containing the display
;           patterns.  6 LEDs require 6 byte data.
;           (IX) contains the pattern for rightmost LED,
;           (IX+5) contains the pattern for leftmost LED.
; Output:   A = internal code of the key pressed.
; Destroyed registers: AF, B, HL, AF', BC', DE'.
;                      All other registers except IY are
;                      also changed during execution, but
;                      they are restored before return.
; Call: SCAN1

SCAN:
	PUSH	IX		;Save IX.
	LD	HL, TEST
	BIT	7, (HL)		;This bit is set if the user
				;has entered illegal key.  The
				;display will be disables as
				;a warning to the user.  This
				;is done by replacing the display
				;buffer pointer IX by BLANK.
	JR	Z, SCPRE
	LD	IX, BLANK

; Wait until all keys are released for 40 ms.
; (The execution time of SCAN1 is 10 ms,
; 40 = 10 * 4.)

SCPRE	LD	B, 4
SCNX	CALL	SCAN1
	JR	NC, SCPRE	;If any key is pressed re-load
				;the debounce counter B by 4.
	DJNZ	SCNX
	RES	7, (HL)		;Clear error-flag.
	POP	IX		;Restore original IX.

; Loop until any key is pressed.

SCLOOP	CALL	SCAN1
	JR	C, SCLOOP

; Convert the key-position-code returned by SCAN1 to
; key-internal-code.  This is done by table-lookup.
; The table used is KEYTAB.

KEYMAP	LD	HL, KEYTAB
	ADD	A, L
	LD	L, A
	LD	A, (HL)
	RET
;
;***********************************************************
; Function: Scan keyboard and display one cycle.
;           Total execution time is about 10 ms (exactly
;           9.95 ms, 17812 clock states @ 1.79 MHz).
; Input: Same as SCAN.
; Output:  i) no key during one scan
;                  Carry flag -- 1
;         ii) key pressed during one scan
;                  Carry flag -- 0,
;                  A -- position code of the key pressed.
;                  If more than one key is pressed, A
;                  contains the largest position-code.
;                  (This key is the last key scanned.)
; Destroyed registers: AF, AF', BC', DE' (see SCAN comments)
; CALL: none.

SCAN1:
;In hardware, the display and keyboard are
;arranged as a 6 by 6 matrix.  Each column
;corresponds to one LED and six key buttons.
;In normal operation, at most one column is
;active.  The pattern of the active LED is the
;data output on port C of 8255.  The data input
;from bit 0-5 of port A are the status of key
;buttons in the active column.  All signals on
;I/O port are active low.

	SCF			;Set carry flag.
	EX	AF, AF'
	EXX

;Carry flag of F' is used to return the status of
;the keyboard.  If any key is pressed during one
;scan, the flag is reset; otherwise it is set.
;Initially, this flag is set.  A' register is used
;to store the position-code of the key pressed.
;In this routine, 36 key positions are checked one
;by one.  C register contains the code of the key
;being checked.  The value of C is 0 at the beginning
;and is increased by 1 after each check.  So the code
;ranges from 0 to 23H (total 36 positions).  On each
;check, if the input bit is 0 (key pressed), C register
;is copied into A'.  The carry flag of F' is set also.
;When some key is detected, the key positions after
;this key will still be checked.  So if more than
;one key are pressed during one scan, the code of the
;last one will be returned.

	LD	C, 0		;Initial position code
	LD	E, 11000001B	;Scan from rightmost digit
	LD	H, 6
				;to the active column.
KCOL	LD	A, E
	OUT	(DIGIT), A	;Activate one column.
	LD	A, (IX)
	OUT	(SEG7), A
	LD	B, COLDEL
	DJNZ	$		;Delay 1.5 ms per digit.
	XOR	A		;Deactivate all display segments
	OUT	(SEG7), A
	LD	A, E
	CPL
	OR	11000000B
	OUT	(DIGIT), A
	LD	B, 6		;Each column has 6 keys.
	IN	A, (KIN)	;Now, bit 0-5 of A contain
				;the status of the 6 keys
				;in the active column.
	LD	D, A		;Store A into D.
KROW	RR	D		;Rotate D 1 bit right, bit 0
				;of D will be rotated into
				;carry flag.
	JR	C, NOKEY	;Skip next 2 instructions
				;if the key is not pressed.
				;The next 2 instructions
				;store the current position-code
				;into A' and reset carry flag
				;of F' register.
	LD	A, C		;Key-in, get key position.
	EX	AF, AF'		;Save A & Carry in AF'.
NOKEY	INC	C		;Increase current key-code by 1.
	DJNZ	KROW		;Loop until 6 keys in the
				;active columns are checked.
	INC	IX
	LD	A, E
	AND	00111111B
	RLC	A
	OR	11000000B
	LD	E, A
	DEC	H
	JR	NZ, KCOL
	LD	DE, -6
	ADD	IX, DE		;Get original IX.
	EXX
	EX	AF, AF'
	RET
;
;***********************************************************
; Function: Convert the 2 byte data stored in DE to
;           7-segment display format.  The output is stored
;           in the address field of DISPBF (display buffer),
;           most significant digit in DISPBF+5.
;           This routine is usually used by monitor only.
; Destroyed registers: AF, HL.
; Call: HEX7SG

ADDRDP:
	LD	HL, DISPBF+2
	LD	A, E
	CALL	HEX7SG
	LD	A, D
	CALL	HEX7SG
	RET
;
;***********************************************************
; Function: Convert the data stored in A to 7-segment
;           display format.  1 byte is converted to 2
;           digits.  The result is stored in the data
;           field of display buffer (DISPBF).
;           This routine is usually used by monitor only.
; Destroyed registers: AF, HL.
; Call: HEX7SG

DATADP:
	LD	HL, DISPBF
	CALL	HEX7SG
	RET
;
;***********************************************************
; Function: Convert binary data to 7-segment display
;           format.
; Input: 1 byte in A register
;        HL points to the result buffer.
; Output: Pattern for 2 digits.  Low order digit in (HL),
;         high order digit in (HL+1).
;         HL becomes HL+2.
; Destroyed registers: AF, HL.
; Call: HEX7

HEX7SG:
	PUSH	AF
	CALL	HEX7
	LD	(HL), A
	INC	HL
	POP	AF
	RRCA
	RRCA
	RRCA
	RRCA
	CALL	HEX7
	LD	(HL), A
	INC	HL
	RET
;
;***********************************************************
; Function: Convert binary data to 7-segment display
;           format.
; Input:  A -- LSB 4 bits contains the binary data
; Output: A -- display pattern for 1 digit.
; Destroyed registers: AF
; Call: none

HEX7:
	PUSH	HL
	LD	HL, SEGTAB
	AND	0FH
	ADD	A, L
	LD	L, A
	LD	A, (HL)
	POP	HL
	RET
;
;
;***********************************************************
; Function: RAM 1800-1FFF self-check.
; Input: none
; Output: none
; Destroyed registers: AF, BC, HL
; Call: RAMCHK

RAMTEST:
	LD	HL, 1800H
	LD	BC, 0800H
RAMT	CALL	RAMCHK
	JR	Z, TNEXT
	HALT			;If error
TNEXT	CPI
	JP	PE, RAMT
	RST	00H		;Display 'HELLO '.
;
;***********************************************************
;Monitor ROM self-check.  Add the data of address
;0000 - 0800.  If the sum equals to 0, reset the monitor
;and display 'HELLO '.  If the sum is not 0, which
;indicates error, HALT. Summed by adding without carry.
;Input: none.
;Output: none.
;Destroyed registers: AF, BC, HL.
;Call: SUM.

ROMTEST:
	LD	HL, 0
	LD	BC, 0800H
	CALL	SUM
	JR	Z, SUMOK
	HALT			;If error.
SUMOK	RST	00H		;Display 'HELLO '.
INI3	LD	(POWERUP), A	;Load power-code into (POWERUP).
				;The monitor uses the location to decide
				;wether a reset signal is on power-up.
	LD	A, 55H
	LD	(BEEPSET), A
	LD	A, 44H
	LD	(FBEEP), A	;Beep frequency when key is pressed.
	LD	HL, TBEEP
	LD	(HL), 2FH	;Time duration of beep when
	INC	HL
	LD	(HL), 0
				;key is pressed.
	JP	INI4

BEEP	PUSH	AF
	LD	HL, FBEEP
	LD	C, (HL)
	LD	HL, (TBEEP)
	LD	A, (BEEPSET)
	CP	55H
	JR	NZ, NOTONE	;There's no beep sound when the key is
				;pressed if data of (BEEPSET) is not 55H.
	CALL	TONE
NOTONE:
	POP	AF
	JP	KEYEXEC		;After a key is detected, determine
				;what action should the monitor take.
				;KEYEXEC uses the next 3 fators
				;to get the entry point of proper
				;service routine: key-code, STATE
				;and STMINOR (Minor-State).
; Below are the branch tables for each key and
; state.  The first entry of each table is
; a base address, other entrys are the offset to
; this address.  Offset is only one byte long,
; which is much shorter than the 2-byte address.
; This can save the monitor code space.

KSUBFUN	.ORG	0737H
	.WORD	KINC
	.BYTE	-KINC+KINC
	.BYTE	-KINC+KDEC
	.BYTE	-KINC+KGO
	.BYTE	-KINC+KSTEP
	.BYTE	-KINC+KDATA
	.BYTE	-KINC+KSBR
	.BYTE	-KINC+KINS
	.BYTE	-KINC+KDEL
KFUN	.WORD	KPC
	.BYTE	-KPC+KPC
	.BYTE	-KPC+KADDR
	.BYTE	-KPC+KCBR
	.BYTE	-KPC+KREG
	.BYTE	-KPC+KMV
	.BYTE	-KPC+KRL
	.BYTE	-KPC+KWT
	.BYTE	-KPC+KRT
HTAB	.WORD	HFIX
	.BYTE	-HFIX+HFIX
	.BYTE	-HFIX+HAD
	.BYTE	-HFIX+HDA
	.BYTE	-HFIX+HRGFIX
	.BYTE	-HFIX+HMV
	.BYTE	-HFIX+HRL
	.BYTE	-HFIX+HWT
	.BYTE	-HFIX+HRT
	.BYTE	-HFIX+HRGAD
	.BYTE	-HFIX+HRGDA
ITAB	.WORD	IFIX
	.BYTE	-IFIX+IFIX
	.BYTE	-IFIX+IAD
	.BYTE	-IFIX+IDA
	.BYTE	-IFIX+IRGFIX
	.BYTE	-IFIX+IMV
	.BYTE	-IFIX+IRL
	.BYTE	-IFIX+IWT
	.BYTE	-IFIX+IRT
	.BYTE	-IFIX+IRGAD
	.BYTE	-IFIX+IRGDA
DTAB	.WORD	DFIX
	.BYTE	-DFIX+DFIX
	.BYTE	-DFIX+DAD
	.BYTE	-DFIX+DDA
	.BYTE	-DFIX+DRGFIX
	.BYTE	-DFIX+DMV
	.BYTE	-DFIX+DRL
	.BYTE	-DFIX+DWT
	.BYTE	-DFIX+DRT
	.BYTE	-DFIX+DRGAD
	.BYTE	-DFIX+DRGDA
GTAB	.WORD	GFIX
	.BYTE	-GFIX+GFIX
	.BYTE	-GFIX+GAD
	.BYTE	-GFIX+GDA
	.BYTE	-GFIX+GRGFIX
	.BYTE	-GFIX+GMV
	.BYTE	-GFIX+GRL
	.BYTE	-GFIX+GWT
	.BYTE	-GFIX+GRT
	.BYTE	-GFIX+GRGAD
	.BYTE	-GFIX+GRGDA

; Key-posistion-code to key-internal-code conversion table.

KEYTAB:
K0	.BYTE	03H	;HEX_3
K1	.BYTE	07H	;HEX_7
K2	.BYTE	0BH	;HEX_B
K3	.BYTE	0FH	;HEX_F
K4	.BYTE	20H	;NOT USED
K5	.BYTE	21H	;NOT USED
K6	.BYTE	02H	;HEX_2
K7	.BYTE	06H	;HEX_6
K8	.BYTE	0AH	;HEX_A
K9	.BYTE	0EH	;HEX_E
K0A	.BYTE	22H	;NOT USED
K0B	.BYTE	23H	;NOT USED
K0C	.BYTE	01H	;HEX_1
K0D	.BYTE	05H	;HEX_5
K0E	.BYTE	09H	;HEX_9
K0F	.BYTE	0DH	;HEX_D
K10	.BYTE	13H	;STEP
K11	.BYTE	1FH	;TAPERD
K12	.BYTE	00H	;HEX_0
K13	.BYTE	04H	;HEX_4
K14	.BYTE	08H	;HEX_8
K15	.BYTE	0CH	;HEX_C
K16	.BYTE	12H	;GO
K17	.BYTE	1EH	;TAPEWR
K18	.BYTE	1AH	;CBR
K19	.BYTE	18H	;PC
K1A	.BYTE	1BH	;REG
K1B	.BYTE	19H	;ADDR
K1C	.BYTE	17H	;DEL
K1D	.BYTE	1DH	;RELA
K1E	.BYTE	15H	;SBR
K1F	.BYTE	11H	;-
K20	.BYTE	14H	;DATA
K21	.BYTE	10H	;+
K22	.BYTE	16H	;INS
K23	.BYTE	1CH	;MOVE
;
;
;
;
HELLO	.BYTE	000H		;' '
	.BYTE	0BDH		;'O'
	.BYTE	085H		;'L'
	.BYTE	085H		;'L'
	.BYTE	08FH		;'E'
	.BYTE	037H		;'H'
BLANK	.BYTE	0
	.BYTE	0
	.BYTE	0
	.BYTE	0
ERR_	.BYTE	0
	.BYTE	0
	.BYTE	003H		;'R'
	.BYTE	003H		;'R'
	.BYTE	08FH		;'E'
	.BYTE	002H		;'-'
SYS_SP	.BYTE	01FH		;'P'
	.BYTE	0AEH		;'S'
	.BYTE	002H		;'-'
	.BYTE	0AEH		;'S'
	.BYTE	0B6H		;'Y'
	.BYTE	0AEH		;'S'
ERR_SP	.BYTE	01FH		;'P'
	.BYTE	0AEH		;'S'
	.BYTE	002H		;'-'
	.BYTE	003H		;'R'
	.BYTE	003H		;'R'
	.BYTE	08FH		;'E'
	.BYTE	0
STEPTAB	.BYTE	0AEH		;'S'
	.BYTE	08FH		;'E'
	.BYTE	0B3H		;'D'
	.BYTE	0
	.BYTE	0AEH		;'S'
	.BYTE	0B3H		;'D'
	.BYTE	0
	.BYTE	0
	.BYTE	00FH		;'F'
	.BYTE	0AEH		;'S'
	.BYTE	08FH		;'E'
	.BYTE	0
	.BYTE	00FH		;'F'
	.BYTE	0
REG_	.BYTE	0
	.BYTE	0
	.BYTE	002H		;'-'
	.BYTE	0BEH		;'G'
	.BYTE	08FH		;'E'
	.BYTE	003H		;'R'
RGTAB	.WORD	03F0FH		;'AF'
	.WORD	0A78DH		;'BC'
	.WORD	0B38FH		;'DE'
	.WORD	03785H		;'HL'
	.WORD	03F4FH		;'AF.'
	.WORD	0A7CDH		;'BC.'
	.WORD	0B3CFH		;'DE.'
	.WORD	037C5H		;'HL.'
	.WORD	03007H		;'IX'
	.WORD	030B6H		;'IY'
	.WORD	0AE1FH		;'SP'
	.WORD	0300FH		;'IF'
	.WORD	00F37H		;'FH'
	.WORD	00F85H		;'FL'
	.WORD	00F77H		;'FH.'
	.WORD	00FC5H		;'FL.'
SEGTAB	.BYTE	0BDH		;'0'
	.BYTE	030H		;'1'
	.BYTE	09BH		;'2'
	.BYTE	0BAH		;'3'
	.BYTE	036H		;'4'
	.BYTE	0AEH		;'5'
	.BYTE	0AFH		;'6'
	.BYTE	038H		;'7'
	.BYTE	0BFH		;'8'
	.BYTE	0BEH		;'9'
	.BYTE	03FH		;'A'
	.BYTE	0A7H		;'B'
	.BYTE	08DH		;'C'
	.BYTE	0B3H		;'D'
	.BYTE	08FH		;'E'
	.BYTE	00FH		;'F'
;
;***********************************************************
;SYSTEM RAM AREA:
USERSTK	.ORG	FF9FH
	.BLOCK	16
SYSSTK:	.ORG	FFAFH
STEPBF	.BLOCK	7
DISPBF	.BLOCK	6
REGBF:
USERAF	.BLOCK	2
USERBC	.BLOCK	2
USERDE	.BLOCK	2
USERHL	.BLOCK	2
UAFP	.BLOCK	2
UBCP	.BLOCK	2
UDEP	.BLOCK	2
UHLP	.BLOCK	2
USERIX	.BLOCK	2
USERIY	.BLOCK	2
USERSP	.BLOCK	2
USERIF	.BLOCK	2
FLAGH	.BLOCK	2
FLAGL	.BLOCK	2
FLAGHP	.BLOCK	2
FLAGLP	.BLOCK	2
USERPC	.BLOCK	2
ADSAVE	.BLOCK	2		;Contains addr being displayed now.
BRAD	.BLOCK	2		;Break point address
BRDA	.BLOCK	1		;Data of break point address
STMINOR	.BLOCK	1		;Minor state
STATE	.BLOCK	1		;State
POWERUP	.BLOCK	1		;Power-up initialization
TEST	.BLOCK	1		;Flag, bit 0 -- set when function or
						;subfunction key is hit. Bit 7 --
						;set when illegal key is entered.
ATEMP	.BLOCK	1		;Temporary storage
HLTEMP	.BLOCK	2		;Temporary storage
TEMP	.BLOCK	4		;See comments on routine GDA.
IM1AD	.BLOCK	2		;Contains addr of Opcode 'FF' serv
						;routine. (RST 38H, mode 1 int, etc)
BEEPSET	.BLOCK	1		;Default value is 55H
FBEEP	.BLOCK	1		;Beep frequency
TBEEP	.BLOCK	2		;Time duration of beep
	.END
