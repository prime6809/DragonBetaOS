	ifne	UseSerial
;
; Serial routines using onboard 6551.
;	

SerData		equ	A.T1		; data register
SerReset	equ	A.T1+1		; reset ACIA on write , data = don't care	
SerStatus	equ	A.T1+1		; status on read
SerCmd		equ	A.T1+2		; command register
SerCtrl		equ	A.T1+3		; Control register

SerCtrlStop	equ	$80		; Stop bit 0=1 bit , 1=2 bits
SerCtrlWl8	equ	$00		; 8 bit word
SerCtrlWl7	equ	$20		; 7 bit word
SerCtrlWl6	equ	$40		; 6 bit word
SerCtrlWl5	equ	$60		; 5 bit word
SerCtrlRxClk	equ	$10		; 0=external, 1=baud rate
SerBaud16x	equ	$00		; 16x external clock
SerBaud50	equ	$01		; 50 baud
SerBaud75	equ	$02		; 75 baud
SerBaud109	equ	$03		; 109 baud
SerBaud134	equ	$04		; 134 baud
SerBaud150	equ	$05		; 150 baud
SerBaud300	equ	$06		; 300 baud
SerBaud600	equ	$07		; 600 baud
SerBaud1200	equ	$08		; 1200 baud
SerBaud1800	equ	$09		; 1800 baud
SerBaud2400	equ	$0A		; 2400 baud
SerBaud3600	equ	$0B		; 3600 baud
SerBaud4800	equ	$0C		; 4800 baud
SerBaud7200	equ	$0D		; 7200 baud
SerBaud9600	equ	$0E		; 9600 baud
SerBaud19200	equ	$0F		; 19200 baud

SerDefCtrl	equ	SerCtrlRxClk+SerBaud19200	; Rx clock=baud rate, 9600 baud, 8 bit word, 1 stop bit
	
SerStatIRQ	equ	$80		; IRQ pending...
SerStatDSR	equ	$40		; DSR set
SerStatDCD	equ	$20		; DCD set
SerStatTxE	equ	$10		; 1 when transmit register empty
SerStatRxF	equ	$08		; 1 when receive register full
SerStatOver	equ	$04		; overrun
SerStatFrame	equ	$02		; framing error
SerStatParity	equ	$01		; parity error	
	
SerInit
	clr		SerReset	; reset ACIA
		
	lda		#SerDefCtrl	; program control register
	sta		SerCtrl
	
	lda		#$0B		; No parity, no interrupt
	sta		SerCmd		; send it.
	
	rts
	
;
; Write character in A to serial port.
;	
SerWriteChar
	pshs		a		; save a
	
SerWaitWrite	
	lda		SerStatus	; read status register	
	anda		#SerStatTxE	; ready to transmit?
	
	ifeq		EmulateSer
	beq		SerWaitWrite	; nope, loop again!
	else
	brn		SerWaitWrite	; nope, loop again!
	endc
	
	puls		a		; recover character
	sta		SerData		; send it!
	rts
	
DoSerEOL
	pshs		a		; save a
	lda		#$0d		; output eol....
	bsr		SerWriteChar	
	lda		#$0a		
	bsr		SerWriteChar	
	puls		a,pc		; restore and return

DoSerSpace
	pshs		a		; save a
	lda		#$20		; output space....
	bsr		SerWriteChar	
	puls		a,pc		; restore and return
	
; Output hex byte in a	
SerOutHexByte
	pshs		d		; save regs
	lbsr		HexByte		; convert A->hex in D
	bsr		SerWriteChar	; write MSB
	exg		a,b		; get LSB
	bsr		SerWriteChar	; and write it
	puls		d,pc		; restore & return

; Output hex word in d
SerOutHexWord
	pshs		d		; save regs
	pshs		b		; save lsb
	lbsr		SerOutHexByte	; Output A as hex
	puls		a		; retrieve pushed b into a		
	lbsr		SerOutHexByte	; Output A as hex
	puls		d,pc		; restore & return

; Write str pointed to by x, at co-ordinates in d
; Takes same parameter as WriteStrAt, but ignores the co-ordinates	
SerWriteStrAt
	pshs		x		; save x
	leax		2,x		; skip co-ordinates	
	bsr		SerWriteStr	; write it
	puls		x,pc
	
SerWriteStr
	pshs		x		; save x
SerWriteStrLoop
	lda		,x+		; get next char
	beq		SerWriteStrEnd
	bsr		SerWriteChar	; write it
	bra		SerWriteStrLoop	; do next
SerWriteStrEnd
	puls		x,pc
	else
SerInit
SerWriteChar
SerOutHexByte
SerOutHexWord
SerWriteStrAt
SerWriteStr
	rts
	endc

SerEOL MACRO
	ifne		UseSerial
	lbsr		DoSerEOL
	endc
	endm

SerSpace MACRO
	ifne		UseSerial
	lbsr		DoSerSpace
	endc
	endm
