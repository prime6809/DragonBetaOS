
                use     defsfile

tylg            set     Sbrtn+Objct
atrv            set     ReEnt+rev
rev             set     1

                mod     eom,name,tylg,atrv,start,size

; 0

StackSize	equ	29		; size of temporary stack frame
BUFSIZE		equ	20		; size of buffer
ESC		equ	$1B		; escape character
MaxFunction	equ	$1A		; maximum function number

FID		rmb	1
BufPtr          rmb     2		; pointer into buffer
BufCount        rmb     1		; bytes in buffer
FuncNo          rmb     1		; function number
ParamCount      rmb     1		; count of parameters passed by basic09
ParamSize       rmb     2		
ChanFID         rmb     1		; path id for channels passed to us from basic
Buffer          rmb     BUFSIZE+1	; buffer 
size            equ     0

Pages		equ	Buffer+2	; pages / mode shares buffer space
Mode		equ	Buffer+3

name            FCS     "graphics"      ; OS9 Module name

                FCB     $01

start           lbra    main		; Jump to main dispatcher

                FCB     $00,$1A,$01,$01

                FCS     "GMODE"
                FCB     $02

		FCS     "GPAGE"
                FCB     $03

                FCS     "CSET"
                FCB     $04

                FCS     "GCLEAR"
                FCB     $05

                FCS     "COLOUR"
                FCB     $06

                FCS     "GRAPHICS"
                FCB     $07

                FCS     "TEXT"
                FCB     $08

                FCS     "TRANSLATE"
                FCB     $09

                FCS     "SCALE"
                FCB     $0A

                FCS     "ROTATE"
                FCB     $0B

                FCS     "MOVE"
                FCB     $0C

                FCS     "PLOT"
                FCB     $0D

                FCS     "DRAW"
                FCB     $0E

		FCS     "GGET"
                FCB     $0F

                FCS     "GPUT"
                FCB     $10

                FCS     "PMODE"
                FCB     $11

                FCS     "GSAVE"
                FCB     $12

                FCS     "GLOAD"
                FCB     $13

                FCS     "PAINT"
                FCB     $14

                FCS     "CIRCLE"
                FCB     $15

                FCS     "GPRINT"
                FCB     $16

                FCS     "INKEY"
                FCB     $17

                FCS     "CHKRDY"
                FCB     $18

                FCS     "MOUSE"

; Handler table entries are 5 bytes long :-
;
; offset	length	purpose
; 0		1	Function number to be passed to graphics driver
; 1		2	Word offset (from beginning of table) of handler routine
; 3		2	Word pionter to filename to open or zero (they're all zero!).
;
		
HandlerTab      FCB     $61				; gmode
                FDB     L0194-HandlerTab,0		
HandlerTab1     FCB     $62				; gpage
                FDB     L0194-HandlerTab,$0000
                FCB     $63				; gmap
                FDB     L0194-HandlerTab,$0000
                FCB     $64				; gset
                FDB     L0194-HandlerTab,$0000
                FCB     $65				; gclear
                FDB     L0194-HandlerTab,$0000
                FCB     $66				; gcolour
                FDB     L0194-HandlerTab,$0000
                FCB     $4F				; text
                FDB     L01D4-HandlerTab,$0000
                FCB     $67				; graphics
                FDB     L0194-HandlerTab,$0000
                FCB     $68				; translate
                FDB     L0194-HandlerTab,$0000
                FCB     $69				; scale
                FDB     L0194-HandlerTab,$0000
                FCB     $6A				; rotate
                FDB     L0194-HandlerTab,$0000
                FCB     $6B				; move
                FDB     L0194-HandlerTab,$0000
                FCB     $6C				; plot
                FDB     L024B-HandlerTab,$0000
                FCB     $6D				; draw
                FDB     L01DF-HandlerTab,$0000
                FCB     $6E				; gget
                FDB     L01DF-HandlerTab,$0000
                FCB     $6F				; gput
                FDB     L0194-HandlerTab,$0000
                FCB     $00				
                FDB     L038B-HandlerTab,$0000
                FCB     $01
                FDB     L038B-HandlerTab,$0000
                FCB     $70				; pmode
                FDB     L0194-HandlerTab,$0000
                FCB     $71				; gsave
                FDB     L0194-HandlerTab,$0000
                FCB     $72				; gload
                FDB     L024B-HandlerTab,$0000	
                FCB     $00				
                FDB     L02AE-HandlerTab,$0000
                FCB     $00				; chkrdy
                FDB     ChkRdy-HandlerTab,$0000		
                FCB     $00				; mouse
                FDB     Mouse-HandlerTab,$0000
	
HandlerLen	equ	HandlerTab1-HandlerTab	

; On entry from Basic09 the stack has the following layout :
;
; SP -> 		return address		2 bytes
;       		Parameter count		2 bytes 
; for each param ->	Address of param	2 bytes
;		 ->	Size of param		2 bytes
;
; The first parameter is the routine number to call.

main           	leay    2,S		; point y at parameter count
                pshs    U		; save it
                leas    <-StackSize,S	; make some room on stack
                leau    ,S		; point U at it
                clr     ,U		; clear first byte of stack frame
                lda     1,Y		; load lsb of param count
                beq     ErrorExit	; branch if zero

                deca			; make param count zero based		
                sta     ParamCount,U	; save it
                ldd     [<2,Y]		; get value of first param (function no)
                cmpb    #MaxFunction	; Greater than max function ?
                bhi     ErrorExit	; branch if higher

                tstb			; Function 0 invalid
                beq     ErrorExit	; branch if zero

                decb			; make zero based
                lda     #HandlerLen	; length of table entry
                mul			; multiply
                leax    HandlerTab,PCR	; point at table
                leax    D,X		; get offset
                lda     ,X		; function no?
                sta     FuncNo,U	; save it
                ldd     3,X		; get word from table
                beq     L015E		; branch if zero (they all are!)

                bsr     OpenFile	; otherwise open file

                blo     BackToBas	; error, return to basic

                sta     ,U		; save file id 
		
L015E           ldd     1,X		; get offset to handler
                leax    HandlerTab,PCR	; point at table base
                leax    D,X		; calculate routine address
                pshs    X		; save x on stack (for rts below)
                leax    Buffer,U	; reset buffer	
                stx     BufPtr,U
                clr     BufCount,U
                leay    6,Y		; skip param count & first param
                rts			; call routine
		
ErrorExit       comb			; flag error
                ldb     #$38

; return to basic09, closing channel if open
BackToBas       pshs    B,CC		; save regs
                lda     ,U		; open file?
                beq     L017D		; no skip

                os9     I$Close		; otherwise close it
L017D           puls    B,CC		; restore regs
                leas    <StackSize,U	; restore stack pointer
                puls    U		; restore saved u 	
                rts			; return to basic09
		
OpenFile        pshs    X		; save X
                leax    HandlerTab,PCR	; point to handler table
                leax    D,X		; get offset of filename
                lda     #UPDAT.		; open for update
                os9     I$Open		; open it
                puls    PC,X            ; restore and return
		
L0194           bsr     FnParmToBuf	; put ESC, function no + param count in buffer

L0196           lda     ParamCount,U	; get parameter count
                beq     LbsrSendBuf	; no parameters, skip

L019A           bsr     ParamToBuf	; transfer parameter to buffer

                leay    4,Y		; move to next parameter
                dec     ParamCount,U	; decrement parameter count
                bne     L019A		; do next if more parameters

LbsrSendBuf     lbsr    SendBuf		; send buffer to channel

                clrb			; flag no error	
                bra     BackToBas	; return to basic09

ParamToBuf      ldd     2,Y		; get parameter size
                beq     L01BE		; skip if zero

                std     ParamSize,U	; save it		
                ldx     ,Y		; get pointer to parameter in x
L01B0           lda     ,X+		; get a byte from param
                lbsr    AtoBuffer	; put it in buffer

                ldd     ParamSize,U	; get parameter size
                subd    #BufPtr		; decrement it
                std     ParamSize,U	; put it back
                bne     L01B0		; loop if still bytes to go

L01BE           rts

FnParmToBuf     bsr     ESCFnToBuff	; put ESC + function number in buffer

                clra			; Put a null byte in buffer
                lbsr    AtoBuffer

                lda     ParamCount,U	; get parameter count
                lbra    AtoBuffer	; put in buffer

ESCFnToBuff     lda     #ESC		; put ESC in buffer to start sequence
                lbsr    AtoBuffer

                lda     FuncNo,U	; get function no in A
                lbra    AtoBuffer	; put it in the buffer

L01D4           lda     ParamCount,U	; get parameter count
                bne     LbraErrorExit	; branch if nonzero, error, exit

                bsr     ESCFnToBuff	; else put function no in buffer

                bra     LbsrSendBuf	; send it to channel

LbraErrorExit   lbra    ErrorExit

L01DF           lda     ParamCount,U	; get parameter count
                cmpa    #$03		; 3 parameters?
                blo     LbraErrorExit	; less than 3, error, exit

                ldd     [,Y]		; get parameter 1 value
                addd    #1		; add 1
                lsra			; divide d by 2
                rorb
                pshs    D		; save it
		
                ldd     [<4,Y]		; get second paramiter value
                pshs    D		; save on stack
		
                lda     3,S
                mul
                std     ParamSize,U
                lda     1,S
                ldb     2,S
                mul
                tsta
                bne     LbraErrorExit

                exg     A,B
                addd    ParamSize,U
                blo     LbraErrorExit

                std     ParamSize,U
                lda     ,S
                ldb     3,S
                mul
                tsta
                bne     LbraErrorExit

                exg     A,B
                addd    ParamSize,U
                blo     LbraErrorExit

                std     ParamSize,U
                lda     ,S
                ldb     2,S
                mul
                bne     LbraErrorExit

                ldd     ParamSize,U
                cmpd    10,Y
                bhi     LbraErrorExit

                lbsr    FnParmToBuf

                lda     #$02
                pshs    A
L022D           lbsr    ParamToBuf

                leay    4,Y
                dec     ParamCount,U
                dec     ,S
                bne     L022D

                leas    1,S
                lda     ,Y
                lbsr    AtoBuffer

                lda     1,Y
                lbsr    AtoBuffer

                leay    4,Y
                dec     ParamCount,U
                lbra    L0196

L024B           tst     ParamCount,U	; get parameter count
                beq     LbraErrorExit	; exit if zero

                lbsr    FnParmToBuf	; transfer function & param count to buffer

L0252           dec     ParamCount,U	; decrement parameter count
                beq     L025D		; branch if all done

                lbsr    ParamToBuf	; transfer parameter to buffer

                leay    4,Y		; move to next param
                bra     L0252		; loop again

L025D           ldd     2,Y		; get parameter size
                std     ParamSize,U	; save it
                beq     L0278		; zero, terminate buffer

                ldx     ,Y		; get address of parameter
L0265           lda     ,X+		; get a byte from parameter
                bmi     L0278		; terminate if has high bit set

                cmpa    #' '		; is it lower than space?		
                blo     L0278		; yes terminate

                bsr     AtoBuffer	; else put it in the buffer	

                ldd     ParamSize,U	; get parameter size
                subd    #1		; decrement it
                std     ParamSize,U	; put it back
                bne     L0265		; loop again if not zero

L0278           lda     #C$CR		; terminate buffer with CR
                lbsr    AtoBuffer	; send to buffer

                lbra    LbsrSendBuf	; send it to buffer

; write byte in A to the buffer, flush buffer if full, then reset to beginning.
AtoBuffer       pshs    Y,X		; save regs
                ldx     BufPtr,U	; get buffer pointer
                sta     ,X+		; save a in buffer, advance pointer
                stx     BufPtr,U	; save updated pointer
                inc     BufCount,U	; increment buffer count
                lda     BufCount,U	; get buffer count
                cmpa    #BUFSIZE	; smaller than buffer size
                blo     L0292		; lower skip

                bsr     SendBuf

L0292           clrb			; no error
                puls    PC,Y,X          ; restore & return
		
SendBuf         ldb     BufCount,U	; get old buffer count
                clra			 
                sta     BufCount,U	; zero buffer count	
                tfr     D,Y		; get current count in Y
		
                leax    Buffer,U	; reset buffer pointer 	
                stx     BufPtr,U	; to beginning of buffer
                lda     ,U		; get file ID
                bne     L02A6		; is file open, yes use it

                lda     #$01		; otherwise use standard out
L02A6           os9     I$Write		; write current buffer
                lblo    BackToBas		; error....

                rts			; return

L02AE           clr     ChanFID,U	; clear channel id, default to 0????
                lda     ParamCount,U	; get param count
                beq     L02EA		; branch if zero, exit with error

                cmpa    #$02		; 2 params?
                bhi     L02EA		; branch if higher, exit with error

                blo     L02BF		; branch if only one param

                ldd     [<4,Y]		; get value of second param
                stb     ChanFID,U	; save it
		
L02BF           ldx     ,Y		; get pointer to first param
                lda     #$FF		; $FF byte
                sta     ,X		; save in first param
                bsr     L02ED		; check if channel ready	

                blo     L02E6		; error, back to basic

                pshs    Y		; save y
                ldx     ,Y		; get pointer to output buffer
                ldy     #1		; read 1 byte
                lda     ChanFID,U	; get file ID of passed channel
                os9     I$Read		; go read it
                blo     L02E7		; return to basic on error

                puls    Y		; restore y
                ldd     2,Y		; get length of first param 
                cmpd    #1		; is it 1?
                bls     L02E6		; yes, exit back to basic

                lda     #$FF		; terminate buffer
                sta     1,X		
L02E6           clrb			; flag no error
L02E7           lbra    BackToBas

L02EA           lbra    ErrorExit

L02ED           lda     ChanFID,U	; get channel ID
                ldb     #SS.Ready	; check channel ready		
                os9     I$GetStt	; go check
                bhs     L02FC		; no error, exit

                cmpb    #E$NotRdy	; not ready error?		
                orcc    #Carry
                bne     L02E7		; anything other than notready, back to basic

L02FC           rts

ChkRdy          clr     ChanFID,U	; clear channel id
                lda     ParamCount,U	; get parameter count
                beq     L02EA		; none, error, exit

                cmpa    #$02		; 2 parameters?
                bhi     L02EA		; more than 2, error, exit

                blo     L030E		; only 1, skip

                ldd     [<4,Y]		; get value of second param
                stb     ChanFID,U	; save it

L030E           clr     [,Y]		; clear first byte of first param
                bsr     L02ED		; check for channel ready

                blo     L0316		; branch if ready

                com     [,Y]		; coplement parameter byte, flag not ready
L0316           clrb			; no error
                bra     L02E7		; return to basic

; Mouse handler
Mouse           lda     ParamCount,U	; get parameter count
                bne     L0326		; branch if parameters, deal with them

                clra			; path id 0, stdin	
                ldb     #SS.Mouse	; reset mouse co-ordinates
                os9     I$SetStt	; go do it
                lbra    BackToBas	; and back to basic

L0326           cmpa    #$04		; 4 params?
                beq     L032E		; yes, skip

                cmpa    #$02		; 2 params?
                bne     MErrorExit	; no, exit with error

L032E           ldd     2,Y		; get size of first param
                cmpd    #$0002		; 2 bytes?
                bne     MErrorExit	; nope, error	

                ldd     6,Y		; get size of second param
                cmpd    #$0002		; is it 2
                bne     MErrorExit	; nope, error

                ldb     ParamCount,U	; get param count
                cmpb    #$02		; is it 2
                beq     L0354		; skip if so

                ldd     10,Y		; get size of third param
                cmpd    #1		; 1 byte?	
                bne     MErrorExit	; nope, error

                ldd     14,Y		; get size of forth param
                cmpd    #1		; 1 byte?	
                bne     MErrorExit	; nope, error

L0354           pshs    Y		; save y (param pointer)
                clra			; stdout
                ldb     #SS.Mouse	; read mouse co-ordinates?
                os9     I$GetStt	; go get them (x in x, y in y!)
                blo     L02E7		

                pshs    A		; save a
                tfr     Y,D		; save y co-ordinate in d
                ldy     1,S		; recover param pointer from stack
                stx     [,Y]		; save X co-ordinate
                std     [<4,Y]		; save Y co-ordinate
                puls    Y,A		; recover param pointer, saved A
                ldb     ParamCount,U	; get param count
                cmpb    #$02		; only 2 parameters?
                beq     L0384		; yes skip

; I *think* this is reading the buttons and returning them, it would make sense, though 
; it's not documented by the original documentation....

                clr     [<8,Y]		; clear 3rd & 4th parameters
                clr     [<12,Y]
                lsra			; move bottom bit to carry
                bcc     L037E		; button not pressed, skip	

                com     [<8,Y]		; otherwise flag pressed
L037E           lsra			; move next bit into carry
                bcc     L0384		; button not pressed, skip	

                com     [<12,Y]		; flag other button pressed
L0384           clrb			; flag no error
                lbra    L02E7

MErrorExit      lbra    ErrorExit	

L038B           lda     ParamCount,U	; get parameter count
                cmpa    #$01		; check for one parameter only
                bne     MErrorExit	; no, error, exit

                ldd     [,Y]		; get value of first parameter (path no)
                stb     ChanFID,U		; save it
                clra			; pathno 0
                ldb     #SS.Size	; get size
                os9     I$GetStt	; go get status
                blo     L03EF		; branch on error

; Function SS.Size on stdin, returns the current graphic mode :
; Y = MSB = 0 lower 64K page only
;         = 1 both pages
;     LSB = 0 graphics not active
;         = 1 320x256 mode
;         = 2 640x512 mode
; X = number of block in lower page

                stx     Buffer,U	; save pageno
                sty     Pages,U		; save page use / mode
                tst     Mode,U		; in graphics mode?	
                beq     L03EF		; no, skip	

                stx     ParamSize,U	; save block number
                ldb     Mode,U		; get mode
                lda     #$05		; * 5
                mul
                tst     Pages,U		; are we using both pages?
                beq     L03B2		; no, only bottom one

                lslb			; multiply by 2
L03B2           stb     BufCount,U	; save it

L03B4           ldx     ParamSize,U	; beginning block number
                ldb     #$01		; map one block	
                pshs    U		; save u
                os9     F$MapBlk	; map the blocks
                tfr     U,X		; get address into X
                puls    U		; restore U
                bcs     L03EF		; error, exit

                stx     BufPtr,U	; save address
                lda     ChanFID,U	; get file ID
                ldy     #$1000		; 4K
                ldb     FuncNo,U	; is it a read or write?
                bne     L03D4		; it's aread, go do it

                os9     I$Write		; write it
                bra     L03D7		; skip over read

L03D4           os9     I$Read		; read it
L03D7           pshs    U,B,CC		; save regs
                ldu     BufPtr,U	; get address
                ldb     #$01		; one block
                os9     F$ClrBlk	; clear block
		
                puls    U,B,CC		; restore regs
                bcs     L03EF		; error, exit

                ldx     ParamSize,U	; get beginning block no
                leax    1,X		; increment it
                stx     ParamSize,U	; store it back
                dec     BufCount,U	; decrement block count
                bne     L03B4		; if more, process them

                clrb			; flag no error
L03EF           lbra    BackToBas	; back to basic

                emod
eom             equ     *
                end


