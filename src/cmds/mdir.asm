                nam     MDir
                ttl     program         ; module

* Disassembled 1900/00/00 01:15:04 by Disasm v1.5 (C) 1988 by RML

                
                use     defsfile
                
tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     $01
edition		set	5

                mod     eom,name,tylg,atrv,start,size

BUFFSIZE	equ	80

ParamPtr        rmb     2		; parameter pointer
ZerosFlag       rmb     1		; zero supression flag for hex output
BufPtr          rmb     2		; output buffer pointer
DateTimeBuf     rmb     3		; date buffer
TimeBuf         rmb     3		; time buffer
Buffer          rmb     BUFFSIZE	; output buffer
LocalEndPtr     rmb     2
SysStartPtr     rmb     2
ModDirBuf       rmb     4096		; module directory buffer
ModNameBuf      rmb     64		; buffer for module name
ModHeadBuf      rmb     269		; buffer for module head
size            equ     .

name            equ     *
                fcs     /MDir/
                fcb     edition

FieldWidth      fcb     12		; field width for non extended display
LastCol         fcb     48             	; last column start pos for non extended display

ModDirAt        fcs	"   Module Directory at "
Heading         fcs	"Block Offset Size Typ Rev Attr  Use Module Name"
Underline       fcs	"----- ------ ---- --- --- ---- ---- ------------"
Lock            fcs	"Lock "

start           equ     *
                pshs    u		; save u
                
		leau    >ModNameBuf,u	; point u at buffer
L0095           clr     ,-u		; clear a byte, decrement u
                cmpu    ,s		; reached beginning of data area ? (saved on stack)
                bhi     L0095		; no, keep going.
		
                puls    u		; restore u
                
		stx     <ParamPtr	; save parameter pointer
                leax    Buffer,u	; point at buffer	
                stx     <BufPtr		; save in buffer pointer
                
		lbsr    WriteBuf	; print a blank line
		
                leay    >ModDirAt,pcr	; point to module dir message
                lbsr    StrYtoBuf	; copy it to buffer
		
                leax    DateTimeBuf,u	; point at time buffer
                os9     F$Time		; get the time from system
		
                leax    TimeBuf,u	; point at time buffer
                lbsr    TimeToDec		; convert time to decimal and output
                lbsr    WriteBuf	; output a blank line
		
                leax    <ModDirBuf,u	; point to buffer for module directory
                pshs    u		; save u
                os9     F$GModDr	; go get mod dir
                sty     <LocalEndPtr	; save local end pointer	
                stu     <SysStartPtr	; save system start pointer
                puls    u		; restore u
		
                leax    -MD$ESize,x				
                ldy     <ParamPtr	; get parameter pointer		
                lda     ,y+		; get a byte from params
                eora    #'E'		; is it E
                anda    #$DF		; mask it
                bne     L0119		; no, skip ahead
		
                lbsr    WriteBuf		; output a blank line
                leay    >Heading,pcr	; point to heading
                lbsr    StrYtoBuf	; copy it to buffer
                lbsr    WriteBuf		; output a blank line
                leay    >Underline,pcr	; output the underline for the heading
                lbsr    StrYtoBuf	; copy it to buffer
                lbsr    WriteBuf		; and a blank line.
	
                leax    <ModDirBuf,u	; point at copy of module dir
                lbra    CheckEnd	; check end of module dir
		
L00F4           lbsr    CalcBufAddr	; calculate buffer address
                beq     L0119		; zero, skip ahead
		
                lbsr    GetModName	; get module name
                lbsr    StrYtoBuf	; put it in buffer
L00FF           lbsr    OutSpace	; and a space
                ldb     <BufPtr+1	; last byte of output pointer
                subb    #11		
                cmpb    >LastCol,pcr	; at last column?
                bhi     L0116		; yes do an eol, and reset
		
L010C           subb    >FieldWidth,pcr	; take field width
                bhi     L010C		; loop until begining of line
                bne     L00FF		; not an exact feild width add a space, and try again
                bra     L0119		; exact field width, skip and output next entry

L0116           lbsr    WriteBuf		; at last column, do an eol and continue

L0119           leax    MD$ESize,x	; move to next entry
                cmpx    <LocalEndPtr	; done all?
                bcs     L00F4		; nope, loop and do next
                lbsr    WriteBuf	; print blank line
                bra     ExitNoError	; and exit

L0124           lbsr    CalcBufAddr	; get local buffer address
                beq     L0187		; if blank, it's unused
		
                tfr     d,y		; transfer to y
                ldd     ,y			
                bsr     OutHexWord	; output block number
                lbsr    OutSpace	; output 2 spaces
                lbsr    OutSpace
                
		ldd     MD$MPtr,x	; get module pointer
                bsr     OutHexWord	; output it
                lbsr    OutSpace	; output space
                lbsr    GetModName	; get module name
		
                leay    >ModHeadBuf,u	; point to module header buffer
                ldd     M$Size,y	; get module size
                bsr     OutHexWord	; output it + space
                lbsr    OutSpace
		
                lda     M$Type,y	; get module type
                bsr     OutHexSpace		; output it + space
                lbsr    OutSpace
		
                lda     M$Revs,y	; get module revision 
                anda    #$0F		; mask out type bits
                bsr     OutHexSpace	; print it
		
                ldb     M$Revs,y	; get module type
                lda     #'r'		; reentrant
                lbsr    OutputFlag
                
		lda     #'?'		; writable
                lbsr    OutputFlag
		
                lda     #'?'		; undefined in OS-9, or 6309 in Nitros
                bsr     OutputFlag
		
                lda     #'?'		; undefined
                bsr     OutputFlag
                bsr     OutSpace
		
                ldd     MD$Link,x	; get link count
                cmpd    #$FFFF		; is it -1?
                bne     L017D		; no skip
		
                leay    >Lock,pcr	; point to locked message
                bsr     StrYtoBuf	; copy it to buffer
                bra     L017F		; skip
		
L017D           bsr     OutHexWord	; output link count

L017F           leay    >ModNameBuf,u	; point to module name buffer
                bsr     StrYtoBuf	; copy it to output buffer
                bsr     WriteBuf	; output it
L0187           leax    MD$ESize,x	; move to next entry

CheckEnd        cmpx    <LocalEndPtr	; reached end of mod dir?
                bcs     L0124		; nope, keep going

ExitNoError     clrb			; no error
                os9     F$Exit		; exit
		
OutHexWord      bsr     DoHexOutStart
                tst     <ZerosFlag
                bne     L0199
                dec     <ZerosFlag
L0199           tfr     b,a
                bsr     DoHexOut
                bra     OutSpace

OutHexSpace     bsr     DoHexOutStart
                bra     OutSpace

; hex conversion
DoHexOutStart   clr     <ZerosFlag	; clear flag 
DoHexOut        pshs    a		; save a
                lsra			; move top nibble to bottom nibble bits
                lsra
                lsra
                lsra
                bsr     L01B9		; convert to ascii
                tst     <ZerosFlag	; are we outputting zeros?
                bpl     L01B5		
		
                lda     #$01
                sta     <ZerosFlag
		
L01B5           lda     ,s+
                anda    #$0F
L01B9           tsta
                beq     L01BE
                sta     <ZerosFlag
L01BE           tst     <ZerosFlag
                bmi     L01C4
                bne     L01C8
L01C4           lda     #$20
                bra     StoreAInBuf
L01C8           adda    #'0'
                cmpa    #'9'
                bls     StoreAInBuf
                adda    #7
                bra     StoreAInBuf
		
OutSpace        lda     #$20

StoreAInBuf     pshs    x               ; save x
                ldx     <BufPtr       	; point x at buffer   
                sta     ,x+             ; save a in buffer
                stx     <BufPtr         ; save buffer pointer
                puls    pc,x            ; restore and return
		
OutputFlag      rolb			; move msb to carry
                bcs     StoreAInBuf	; if bit set, store char passed in a
                lda     #'.'		; else period
                bra     StoreAInBuf	; do it

StrYtoBuf       lda     ,y 		; get byte from message             
                anda    #$7F            ; reset bit 7
                bsr     StoreAInBuf     ; store it in buff      
                lda     ,y+             ; get next char from string
                bpl     StrYtoBuf       ; loop again if not end of string
                rts

WriteBuf       	pshs    y,x,a		; save regs
                lda     #C$CR		; Put a CR in buffer		
                bsr     StoreAInBuf
		
                leax    Buffer,u	; point at beginning of buffer
                stx     <BufPtr		; put in buffer pointer
                
		ldy     #BUFFSIZE	; Write the whole buffer
                lda     #$01		; standard out	
                os9     I$WritLn	; write it
                puls    pc,y,x,a	; restore and return	

TimeToDec       bsr     ByteToDec	; convert byte to decimal, hours 
                bsr     L0209		; and again for minutes, seperated by :
L0209           lda     #':'		; then fall through for seconds.....
                bsr     StoreAInBuf

; convert the byte at x to decimal and output it to the buffer.
; this code works thus :
; take byte and subtract 100 repeatedly till it underflows....to generate 100s digit
; then add 10 repeatedly till it is again > 0 to calculate the 10's digit
; the remainder is then the units.
;
ByteToDec       ldb     ,x+		; get byte from time		
                lda     #'/'		; init digit, compensate for immediate inc....
L0211           inca			; inc 100s digit, move to next
                subb    #100		; calculate hundreds		
                bcc     L0211		; keep going whilst > 100
		
                cmpa    #'0'		; > 900 
                beq     L021C		; yes.....		
		
                bsr     StoreAInBuf	; store hundreds
L021C           lda     #':'		; prime 10s, compensate for immediate dec
L021E           deca			; dec 10s digit
                addb    #10		; add 10
                bcc     L021E		; keep going till > 0
		
                bsr     StoreAInBuf	; store 10s digit in buffer
                tfr     b,a		; now units left in b, transfer to a
                adda    #'0'		; convert to ascii		
                bra     StoreAInBuf	; store units
		
GetModName      pshs    u,x		; save regs
                bsr     CalcBufAddr	; calculate buffer address of current entry
                ldx     MD$MPtr,x	; get module pointer
                ldy     #$000D		; copy 13 bytes (module header)
                leau    >ModHeadBuf,u	; point at module header buffer
                os9     F$CpyMem	; get module name
		
                pshs    d		; save d	
                ldd     M$Name,u	; get module name offset		
                leax    d,x		; add to module pointerm so now points to name
                puls    d		; restore block number
		ldu     $02,s		; recover statics pointer
                leau    >ModNameBuf,u	; point at module name buffer
                ldy     #$0040		; get 64 bytes.....
                os9     F$CpyMem	; go copy the memory
                tfr     u,y		; y now pointer to name buff
                puls    pc,u,x		; restore and return

; calculate local buffer address for current mdir entry (DAT image ptr)
; returned in D
CalcBufAddr     ldd     ,x		; get DAT image pointer
                beq     L0266		; if zero, skip empty slot....
		
                pshs    y		; save y
                leay    <ModDirBuf,u	; Y=local mdir pointer
                pshs    y		; save on stack
                subd    <SysStartPtr	; calculate offset 
                addd    ,s++		; add that to stacked mdir pointer
                puls    y		; restore y
L0266           rts
                emod
eom             equ     *
                end
