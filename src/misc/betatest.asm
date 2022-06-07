;
; Dragon Data, Dragon Beta test ROM.
; (C) 1984 DragonData Ltd.
;
; Disassembled & commented 2022-04 
; Phill Harvey-Smith
;

		use	defsfile

IOBase		equ	$FC00		; base address of I/O page.
DDRMask		equ	$04		; DDR access bit in PIA control register

VSyncFlag	equ	%01000000	; CB2 in PIA

; Bitmasks for keyboard 

KInLoad		equ	%00001000	; input load, toggle CB2 
KAnyKey		equ	%00000100	; Any key down, PB2
KClkDataIn	equ	%00010000	; Clock out / Data in, PB4
KDataOut	equ	%00100000	; Data out, PB5

L0000   equ $0000
L0001   equ $0001
L0002   equ $0002
L0004   equ $0004
L0006   equ $0006
L0008   equ $0008
L000A   equ $000A
L000C   equ $000C
L000E   equ $000E
L0010   equ $0010
L0012   equ $0012
L0014   equ $0014
L0016   equ $0016
L0018   equ $0018
L001A   equ $001A
L001C   equ $001C
L001E   equ $001E
L0020   equ $0020
L0021   equ $0021
L0022   equ $0022
L0023   equ $0023
L0024   equ $0024
L0025   equ $0025
L0026   equ $0026
L0027   equ $0027
L0041   equ $0041
L0042   equ $0042
L0044   equ $0044
L0045   equ $0045
L0047   equ $0047
L004C   equ $004C
L004E   equ $004E
L004F   equ $004F
L0052   equ $0052
L0055   equ $0055
L006E   equ $006E
L0080   equ $0080
L0081   equ $0081
L00A0   equ $00A0
L00A1   equ $00A1
L00A2   equ $00A2
L00A3   equ $00A3
L00C0   equ $00C0
L00C1   equ $00C1
L00C2   equ $00C2
L00C3   equ $00C3
L00E3   equ $00E3
L00FE   equ $00FE
L00FF   equ $00FF
L41B8   equ $41B8
L4478   equ $4478
L4578   equ $4578
L46B8   equ $46B8
L4711   equ $4711
L48B8   equ $48B8
L4978   equ $4978
L520A   equ $520A
L5278   equ $5278
L5578   equ $5578
LBFDF   equ $BFDF
LD000   equ $D000
LD001   equ $D001
LD002   equ $D002
LD00A   equ $D00A
LD014   equ $D014
LD015   equ $D015
LD017   equ $D017
LD018   equ $D018
LD031   equ $D031
LD033   equ $D033
LD040   equ $D040
LD041   equ $D041
LD042   equ $D042
LD044   equ $D044
LD100   equ $D100
LD102   equ $D102
LD104   equ $D104
LD106   equ $D106
LD108   equ $D108
LD140   equ $D140
LD142   equ $D142
LD144   equ $D144
LD146   equ $D146
LD148   equ $D148
LD14A   equ $D14A
LD14C   equ $D14C
LD14E   equ $D14E
LD200	equ $d200
LD300   equ $D300
LD302   equ $D302
LD306   equ $D306
LD308   equ $D308
LD30A   equ $D30A
LD30C   equ $D30C
LD4FE   equ $D4FE
LD500   equ $D500
LD502   equ $D502
LD504   equ $D504
LD506   equ $D506
LD508   equ $D508
LD50A   equ $D50A
LD50C   equ $D50C
LD50E   equ $D50E
LD510   equ $D510
LD512   equ $D512
LD514   equ $D514
LD516   equ $D516
LD518   equ $D518
LD51A   equ $D51A
LD51C   equ $D51C
LD51E   equ $D51E
LD600   equ $D600
LD700   equ $D700
LDFFC   equ $DFFC
LDFFE   equ $DFFE

        org     $E000
LE000
        FCB    $00,$00,$00,$00,$00,$20,$07,$00
        FCB    $22,$20,$01,$26,$94,$2E,$28,$37
        FCB    $00,$00,$00,$00,$00,$20,$07,$00
        FCB    $22,$20,$01,$26,$98,$5A,$50,$6F
        FCB    $00,$00,$00,$00,$00,$20,$06,$03
        FCB    $46,$40,$01,$4D,$94,$2E,$28,$37
        FCB    $00,$00,$00,$00,$00,$60,$06,$03
        FCB    $46,$40,$01,$4D,$98,$5A,$50,$6F
        FCB    $00,$00,$00,$00,$00,$20,$07,$00
        FCB    $22,$20,$01,$26,$98,$5A,$50,$6F

LE050
        FCB    $00,$00,$00,$00,$0B,$60,$09,$50
        FCB    $1B,$19,$03,$1E,$94,$2E,$28,$37

        FCB    $00,$00,$00,$00,$0B,$60,$09,$50
        FCB    $1B,$19,$03,$1E,$98,$5A,$50,$6F

LE070
        FCB    $00,$01,$02,$03,$04,$05,$06,$07
        FCB    $08,$09,$0A,$0B,$0C,$1F,$FE,$FF
        FCB    $01,$02,$03,$04,$05,$06,$07,$08
        FCB    $09,$0A,$0B,$0C,$0D,$1F,$FE,$FF
        FCB    $0E,$0F,$10,$11,$12,$13,$14,$15
        FCB    $16,$17,$18,$19,$1A,$1F,$FE,$FF
        FCB    $1B,$1C,$1D,$1E,$00,$01,$02,$03
        FCB    $04,$05,$06,$07,$08,$1F,$FE,$FF
        FCB    $00,$01,$02,$03,$04,$05,$06,$07
        FCB    $08,$09,$0A,$0B,$0C,$1F,$FE,$FF
        FCB    $0E,$0F,$10,$11,$12,$13,$14,$15
        FCB    $16,$17,$18,$19,$1A,$1F,$FE,$FF
        FCB    $1B,$1C,$1D,$1E,$1F,$20,$21,$22
        FCB    $23,$24,$25,$26,$27,$1F,$FE,$FF
        FCB    $28,$29,$2A,$2B,$2C,$2D,$2E,$2F
        FCB    $30,$31,$32,$33,$34,$1F,$FE,$FF
        FCB    $35,$36,$37,$38,$39,$3A,$3B,$3C
        FCB    $3D,$3E,$3F,$00,$01,$1F,$FE,$FF
        FCB    $00,$01,$02,$03,$04,$05,$06,$07
        FCB    $08,$09,$0A,$0B,$0C,$1F,$FE,$FF
        FCB    $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        FCB    $12,$13,$14,$15,$16,$1F,$FE,$FF
        FCB    $5A,$5B,$5C,$5D,$5E,$5F,$60,$61
        FCB    $62,$63,$64,$65,$66,$1F,$FE,$FF
        FCB    $67,$68,$69,$6A,$6B,$6C,$6D,$6E
        FCB    $6F,$70,$71,$72,$73,$1F,$FE,$FF
        FCB    $74,$75,$76,$77,$78,$79,$7A,$7B
        FCB    $7C,$7D,$7E,$7F,$40,$1F,$FE,$FF
        FCB    $67,$68,$69,$6A,$6B,$6C,$6D,$6E
        FCB    $6F,$70,$71,$72,$73,$1F,$FE,$FF
        FCB    $74,$75,$76,$77,$78,$79,$7A,$7B
        FCB    $7C,$7D,$7E,$7F,$02,$1F,$FE,$FF

ResetVector
        orcc    #$50			; disable ints				
        lds     #$dfe0			; setup stack
	
        lda     #$fc			; setup direct page to point to peripheral space
        tfr     a,dp
        
	ldd     #$aa55			; I guess this is testing for a cold boot?
        cmpd    ldffe
        bne     le1a1			; branch on cold boot

        ldd     #$ff00			; another test
        cmpd    ldffc			
        bne     le1a1			; branch if invalid

        lds     #$dffb			; set stack
	
LE190
        sync
        bra     le190

LE193
        sync
        lda     ,x+
        sta     <l00e3
        bra     le193

LE19A
        sync
        lda     <l00e3
        sta     ,x+
        bra     le19a

LE1A1
        ldb     #DDRMask		; access output register on PIAs..... 
        stb     <(DAT.Task-IOBase)+1	; 
        stb     <(A.Mouse-IOBase)+1
        stb     <(A.Mouse-IOBase)+3
        lda     #$c0			; Output value
        sta     <(DAT.Task-IOBase)	; select task 0, keep DMA halted
        sta     <(A.Mouse-IOBase)
        lda     #$02
        sta     <(A.Mouse-IOBase)+2
        
	clrb				; access DDRs of PIAs
        stb     <(DAT.Task-IOBase)+1
        stb     <(DAT.Task-IOBase)+3
        stb     <(A.P-IOBase)+1
        stb     <(A.P-IOBase)+3
        stb     <(A.Mouse-IOBase)+1
        stb     <(A.Mouse-IOBase)+3
        
	decb				; b is now FF......
        lda     #$cf
        sta     <(DAT.Task-IOBase)	; DAT task register
        stb     <(DAT.Task-IOBase)+2	; graphics controll all outputs
        stb     <(A.P-IOBase)		; printer all outputs
        lda     #$18
        sta     <(A.P-IOBase)+2		; printer control lines + keyboard
        stb     <(A.Mouse-IOBase)	; mouse
        lda     #$83
        sta     <(A.Mouse-IOBase)+2
	
        lda     #$07			; setup PIA control registers
        sta     <(DAT.Task-IOBase)+1
        lda     #$1c
        sta     <(DAT.Task-IOBase)+3
        lda     #$3d
        sta     <(A.P-IOBase)+1
        lda     #$3c
        sta     <(A.P-IOBase)+3
        lda     #$0d
        sta     <(A.Mouse-IOBase)+1
        lda     #$04
        sta     <(A.Mouse-IOBase)+3
	
        clrb				; set output registers
        lda     #$c0
        sta     <(DAT.Task-IOBase)	; dat task
        lda     #$08
        sta     <(DAT.Task-IOBase)+2
        lda     #$18
        stb     <(A.P-IOBase)
        sta     <(A.P-IOBase)+2
        lda     #$94
        sta     <(A.Mouse-IOBase)
        lda     #$83
        sta     <(A.Mouse-IOBase)+2
	
        ldx     #LE050			; point to init CRT table
        ldb     #$0f			; this number of registers
LE207
        lda     ,x+			; get a byte
        stb     <(A.Crtc-IOBase)	; set register to set
        sta     <(A.Crtc-IOBase)+1	; set value
        decb				; decrement count
        bge     le207			; loop again, if more

; Initialize the DAT
        ldu     #LE070			; point to DAT page table in ROM
        ldb     #$f0			; initial DAT.Task value, (task 0)
LE215
        stb     <(DAT.Task-IOBase)	; set task to set pages of
        ldy     #DAT.Regs		; point at DAT registers
LE21B
        lda     ,u+			; get a byte from ROM table
        sta     ,y+			; Write to DAT register
        cmpy    #DAT.Regs+$10		; done all?
        blo     le21b			; no loop again

        incb				; move to next task
        bne     le215			; will overflow to 0 when done, loop if not

        lda     #$80			; back to task 0
        sta     <(DAT.Task-IOBase)
	
        ldd     #$aa55			; magic value?
        std     ldffe			; save it
        ldd     #$ff00
        std     ldffc
	
        clra

        lda     <(A.Mouse-IOBase)	; read mouse / disk select
        anda    #$7f			; set b7=0, halt DMA CPU
        sta     <(A.Mouse-IOBase)	; save it back
	
        ldu     #LD200			; point at RAM
        ldx     #LE000			; point at table in ROM
LE245
        lda     ,x			; get a byte
        sta     ,u			; put it in RAM
        leax    1,x			; increment pointers
        leau    1,u
        cmpu    #LD200+$70		; done all
        blo     le245			; nope loop again....

        lda     #$2c			; init some more stuff in RAM
        sta     ld700
        lda     #$08
        sta     ld600
        lda     #$11
        sta     ld100
        lda     #$22
        sta     ld102
        lda     #$44
        sta     ld104
        lda     #$ff
        sta     ld106
        lda     #$00
        sta     ld108
	
        lda     #$04			; switch to task 4
        lbsr    ToTaskinA

        ldd     #$ffff			; set some stuff in that task
        std     ld300
        ldd     #$0000
        std     ld4fe
        ldd     #$ffff
        std     ld500
        ldd     #$f0ff
        std     ld502
        ldd     #$fff0
        std     ld504
        ldd     #$f0f0
        std     ld506
        ldd     #$ff0f
        std     ld508
        ldd     #$f00f
        std     ld50a
        ldd     #$ff00
        std     ld50c
        ldd     #$f000
        std     ld50e
        ldd     #$0fff
        std     ld510
        ldd     #$00ff
        std     ld512
        ldd     #$0ff0
        std     ld514
        ldd     #$00f0
        std     ld516
        ldd     #$0f0f
        std     ld518
        ldd     #$000f
        std     ld51a
        ldd     #$0f00
        std     ld51c
        ldd     #$0000
        std     ld51e
	
        clra				; back to task 0
        lbsr    ToTaskinA

LE2EB
        lbsr    ClsText40		; clear screen & switch to text 40.

        ldx     #$ffff			; set some RAM stuff
        stx     ld031
        clr     ld033
        clr     ld017
        clr     ld018
        clr     ld014
	
        lda     <(A.KBD-IOBase)		; read keyboard?
        anda    #$ef			; force sound bit low?
        sta     <(A.KBD-IOBase)
	
        ldb     #$0a			; loop 10 times 
LE308
        lda     <(A.KBD-IOBase)+1	; get control register side b
        anda    #$FF-KInLoad		; Input load low..... $f7
        sta     <(A.KBD-IOBase)+1
        ora     #KInLoad		; input load high
        sta     <(A.KBD-IOBase)+1
        decb				; decrement
        bne     le308			; next loop

        anda    #$FF-KInLoad		; Input load low
        sta     <(A.KBD-IOBase)+1
        lda     <(A.KBD-IOBase)+1	; read control register
        anda    #$ef
        sta     <(A.KBD-IOBase)		; output to dat regiser????is this correct?
	
        andcc   #$ff-IRQMask		; enable IRQ.
	
; Display initial menu page, wait for a key and execute the selected option.
; If an invalid key is selected display an error message.
	
LE321
        ldx     #Page1Text		; point at text table
LE324
        ldd     ,x			; get screen (offset) pointer
        beq     LE32D			; zero, no more to do, exit loop

        lbsr    WriteStrPos		; display the message

        bra     LE324			; loop again

LE32D
        ldx     #KeyTable1		; Point at key / routine table
        lbsr    lf7de			; go wait for key and check it

        blo     le340			; invalid option, branch

        lbsr    ClsText40		; clear screen

        jsr     [,x]			; go do selected option

        lda     #$10			; set graphics control port
        sta     <(A.GCon-IOBase)	
        bra     le321			; loop again

LE340
        ldx     #MessError		; point at error message
        lbsr    WriteStrPos		; display it

        lbsr    ShortDelay

        ldx     #MessSpaces		; point at spaces
        lbsr    WriteStrPos		; overwrite error message

        bra     le32d			; try reading option again.
	
	

LE351
        ldx     #$d200
        lbsr    lf761

        lda     #$04
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$10
        sta     <l00c2
        lda     ld106
        lbsr    lf6ed

        ldx     #$0001
        ldy     #$0001
LE370
        lbsr    le45c

        leay    1,y
        cmpy    #$013f
        blo     le370

        ldx     #$00fe
        ldy     #$0001
LE382
        lbsr    le45c

        leay    1,y
        cmpy    #$013f
        blo     le382

        ldx     #$0001
        ldy     #$0001
LE394
        lbsr    le45c

        leax    1,x
        cmpx    #$00ff
        blo     le394

        ldx     #$0001
        ldy     #$013e
LE3A5
        lbsr    le45c

        leax    1,x
        cmpx    #$00ff
        blo     le3a5

LE3AF
        ldx     #$0008
        ldy     #$0008
LE3B6
        lbsr    le44c

        leay    1,y
        cmpy    #$0138
        blo     le3b6

        ldx     #$00f7
        ldy     #$0008
LE3C8
        lbsr    le44c

        leay    1,y
        cmpy    #$0138
        blo     le3c8

        ldx     #$0008
        ldy     #$0008
LE3DA
        bsr     le44c

        leax    1,x
        cmpx    #$00f8
        blo     le3da

        ldx     #$0008
        ldy     #$0137
LE3EA
        bsr     le44c

        leax    1,x
        cmpx    #$00f8
        blo     le3ea

        ldx     #$0000
        ldy     #$0000
LE3FA
        bsr     le43e

        leax    1,x
        leay    1,y
        cmpx    #$0100
        blo     le3fa

        ldx     #$00ff
        ldy     #$0000
LE40C
        bsr     le43e

        leay    1,y
        leax    -1,x
        cmpx    #$0000
        bhi     le40c

        ldx     #$0000
        ldy     #$013f
LE41E
        bsr     le43e

        leax    1,x
        leay    -1,y
        cmpx    #$0100
        blo     le41e

        ldx     #$00ff
        ldy     #$013f
LE430
        bsr     le43e

        leax    -1,x
        leay    -1,y
        cmpx    #$0000
        bhi     le430

        lbra    lf713

LE43E
        pshs    y,x
        bsr     le46c

        ldd     ,u
        ora     ,x
        orb     ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE44C
        pshs    y,x
        bsr     le46c

        ldd     ,u
        ora     ,x
        leax    8,x
        andb    ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE45C
        pshs    y,x
        bsr     le46c

        ldd     ,u
        orb     ,x
        leax    8,x
        anda    ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE46C
        pshs    y
        pshs    b,a
        tfr     x,d
        pshs    b
        andb    #$f8
        lda     #$28
        mul
        tfr     d,x
        puls    b
        andb    #$07
        abx
        tfr     y,d
        andb    #$f8
        leax    d,x
        tfr     x,d
        leax    d,x
        tfr     x,u
        tfr     y,d
        andb    #$07
        ldx     #lf803
        abx
        puls    b,a
        puls    y
        rts
	
LE499	
        ldx     #$d200
        lbsr    lf761

        lda     #$04
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$10
        sta     <l00c2
        lda     ld108
        lbsr    lf6ed

        ldx     #$0000
        ldy     #$0000
LE4B8
        bsr     le45c

        leay    1,y
        cmpy    #$0140
        blo     le4b8

        ldx     #$00ff
        ldy     #$0000
LE4C9
        bsr     le45c

        leay    1,y
        cmpy    #$0140
        blo     le4c9

        ldx     #$0000
        ldy     #$0000
LE4DA
        lbsr    le45c

        leax    1,x
        cmpx    #$0100
        blo     le4da

        ldx     #$0000
        ldy     #$013f
LE4EB
        lbsr    le45c

        leax    1,x
        cmpx    #$0100
        blo     le4eb

        lbra    le3af

LE4F8
        ldx     #$d210
        lbsr    lf761

        lda     #$04
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$20
        sta     <l00c2
        tst     ld300
        bne     le524

        ldd     #$0140
        std     ld306
        ldd     #$0110
        std     ld308
        ldd     #$0010
        std     ld302
        jmp     le536

LE524
        ldd     #$0100
        std     ld306
        ldd     #$0154
        std     ld308
        ldd     #$0014
        std     ld302
LE536
        ldy     #$0000
        ldx     le5ae
        stx     ld30a
        ldd     ld302
LE543
        std     ld30c
        ldx     ld30a
        leax    2,x
        stx     ld30a
LE54E
        ldx     #$0000
LE551
        bsr     le580

        pshs    x
        ldx     ld30a
        ldd     ,u
        orb     ,x
        ora     1,x
        std     ,u
        puls    x
        leax    1,x
        cmpx    ld306
        blo     le551

        leay    1,y
        cmpy    ld30c
        blo     le54e

        ldd     ld30c
        addd    ld302
        cmpd    ld308
        blo     le543

        lbra    lf713

LE580
        pshs    b,a
        pshs    y,x
        tst     ld300
        bne     le58b

        exg     x,y
LE58B
        tfr     x,d
        pshs    b
        andb    #$f8
        lda     #$50
        mul
        tfr     d,x
        puls    b
        andb    #$07
        abx
        tfr     y,d
        andb    #$fc
        leax    d,x
        leax    d,x
        tfr     x,d
        leax    d,x
        tfr     x,u
        puls    y,x
        puls    b,a
        rts
LE5AE
        andb    <l00fe
        bitb    <l0000
        bitb    <l0002
        bitb    <l0004
        bitb    <l0006
        bitb    <l0008
        bitb    <l000a
        bitb    <l000c
        bitb    <l000e
        bitb    <l0010
        bitb    <l0012
        bitb    <l0014
        bitb    <l0016
        bitb    <l0018
        bitb    <l001a
        bitb    <l001c
        bitb    <l001e

LE5D0
        ldx     #$d220
        lbsr    lf761

        lda     #$09
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$14
        sta     <l00c2
        lda     ld106
        sta     <l00a0
        lda     ld108
        sta     <l00a1
        lda     ld106
        sta     <l00a2
        lda     ld108
        sta     <l00a3
        ldx     #$0001
        ldy     #$0001
LE5FD
        lbsr    le6ca

        leay    1,y
        cmpy    #$027f
        blo     le5fd

        ldx     #$01fe
        ldy     #$0001
LE60F
        lbsr    le6ca

        leay    1,y
        cmpy    #$027f
        blo     le60f

        ldx     #$0001
        ldy     #$0001
LE621
        lbsr    le6ca

        leax    1,x
        cmpx    #$01ff
        blo     le621

        ldx     #$0001
        ldy     #$027e
LE632
        lbsr    le6ca

        leax    1,x
        cmpx    #$01ff
        blo     le632

LE63C
        ldx     #l0008
        ldy     #l0008
LE643
        lbsr    le6ca

        leay    1,y
        cmpy    #$0278
        blo     le643

        ldx     #$01f7
        ldy     #l0008
LE655
        bsr     le6ca

        leay    1,y
        cmpy    #$0278
        blo     le655

        ldx     #l0008
        ldy     #l0008
LE666
        bsr     le6ca

        leax    1,x
        cmpx    #$01f8
        blo     le666

        ldx     #l0008
        ldy     #$0277
LE676
        bsr     le6ca

        leax    1,x
        cmpx    #$01f8
        blo     le676

        ldx     #l0000
        ldy     #l0000
LE686
        bsr     le6ca

        leay    1,y
        leax    1,x
        cmpx    #$0200
        blo     le686

        ldx     #$01ff
        ldy     #l0000
LE698
        bsr     le6ca

        leay    1,y
        leax    -1,x
        cmpx    #l0000
        bhi     le698

        ldx     #l0000
        ldy     #$027f
LE6AA
        bsr     le6ca

        leax    1,x
        leay    -1,y
        cmpx    #$0200
        blo     le6aa

        ldx     #$01ff
        ldy     #$027f
LE6BC
        bsr     le6ca

        leax    -1,x
        leay    -1,y
        cmpx    #l0000
        bhi     le6bc

        lbra    lf713

LE6CA
        pshs    y,x
        bsr     le6d8

        ldd     ,u
        ora     ,x
        orb     1,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE6D8
        pshs    y,b,a
        pshs    x
        tfr     x,d
        pshs    b
        andb    #$f8
        lda     #$50
        mul
        tfr     d,x
        puls    b
        andb    #$07
        lda     #$02
        mul
        abx
        tfr     y,d
        andb    #$f0
        leax    d,x
        tfr     x,u
        tfr     y,d
        andb    #$0f
        ldx     #$f813
        lslb
        abx
        puls    y
        cmpy    #$0100
        blo     le70d

        ldd     #$5000
        leau    d,u
LE70D
        puls    y,b,a
        rts

LE710	
        ldx     #$d220
        lbsr    lf761

        lda     #$09
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$14
        sta     <l00c2
        lda     ld108
        sta     <l00a0
        lda     ld100
        sta     <l00a1
        lda     ld108
        sta     <l00a2
        lda     ld100
        sta     <l00a3
        ldx     #l0000
        ldy     #l0000
LE73D
        bsr     le6ca

        leay    1,y
        cmpy    #$0280
        blo     le73d

        ldx     #$01ff
        ldy     #l0000
LE74E
        lbsr    le6ca

        leay    1,y
        cmpy    #$0280
        blo     le74e

        ldx     #l0000
        ldy     #l0000
LE760
        lbsr    le6ca

        leax    1,x
        cmpx    #$0200
        blo     le760

        ldx     #l0000
        ldy     #$027f
LE771
        lbsr    le6ca

        leax    1,x
        cmpx    #$0200
        blo     le771

        lbra    le63c

LE77E
        ldx     #$d230
        lbsr    lf761

        lda     #$09
        lbsr    ToTaskinA

        lda     #$34
        sta     <l00c2
        lda     #$0a
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$09
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     ld106
        lbsr    lf6ed

        lda     #$ff
        sta     ld148
        sta     ld14a
        sta     ld14c
        ldx     #$0001
        ldy     #$0001
        ldd     #$01ff
        lbsr    le8ae

        ldy     #$027e
        lbsr    le8ae

        lda     #$00
        sta     ld14c
        ldy     #$0001
        ldd     #$027f
        lbsr    le8ae

        ldx     #$01fe
        lbsr    le8ae

LE7D7
        lda     #$00
        sta     ld148
        ldx     #l0008
        ldy     #l0008
        ldd     #$0278
        lbsr    le8ae

        ldx     #$01f7
        lbsr    le8ae

        lda     #$ff
        sta     ld14c
        ldx     #l0008
        ldd     #$01f8
        lbsr    le8ae

        ldy     #$0277
        lbsr    le8ae

        lda     #$00
        sta     ld14a
        ldx     #$01ff
        ldy     #l0000
        ldd     #$0200
        lbsr    led41

        ldy     #$007f
        ldd     #$0280
        lbsr    led41

        lda     #$00
        sta     ld14c
        ldx     #l0000
        ldy     #l0000
        ldd     #$0200
        lbsr    led41

        ldy     #$007f
        ldd     #$0280
        lbsr    led41

        lbra    lf713

LE83F
        pshs    y,x
        bsr     le86d

        ldd     ,u
        ora     ,x
        orb     ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE84D
        pshs    y,x
        bsr     le86d

        ldd     ,u
        ora     ,x
        leax    8,x
        andb    ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE85D
        pshs    y,x
        bsr     le86d

        ldd     ,u
        orb     ,x
        leax    8,x
        anda    ,x
        std     ,u
        puls    pc,y,x                  ; Pull of PC, effective RTS
LE86D
        pshs    y,b,a
        pshs    x
        tfr     x,d
        pshs    b
        andb    #$f8
        lda     #$50
        mul
        tfr     d,x
        puls    b
        andb    #$07
        abx
        tfr     y,d
        andb    #$f8
        leax    d,x
        tfr     x,u
        tfr     y,d
        andb    #$07
        ldx     #lf803
        abx
        puls    y
        cmpy    #$0100
        blo     le8a0

        lda     #$0a
        lbsr    ToTaskinA

        bra     le8a5

LE8A0
        lda     #$09
        lbsr    ToTaskinA

LE8A5
        tfr     u,d
        lslb
        rola
        tfr     d,u
        puls    y,b,a
        rts
LE8AE
        pshs    y,x
        pshs    b,a
        std     ld14e
LE8B5
        tst     ld14a
        bne     le8bf

        bsr     le83f

        jmp     le8cb

LE8BF
        tst     ld148
        bne     le8c9

        bsr     le84d

        jmp     le8cb

LE8C9
        bsr     le85d

LE8CB
        tst     ld14c
        bne     le8d9

        leay    1,y
        cmpy    ld14e
        jmp     le8de

LE8D9
        leax    1,x
        cmpx    ld14e
LE8DE
        blo     le8b5

        puls    b,a
        puls    y,x
        rts

LE8E5	
        ldx     #$d230
        lbsr    lf761

        lda     #$09
        lbsr    ToTaskinA

        lda     #$34
        sta     <l00c2
        lda     #$0a
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$09
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     ld108
        lbsr    lf6ed

        lda     #$ff
        sta     ld148
        sta     ld14a
        sta     ld14c
        ldx     #l0000
        ldy     #l0000
        ldd     #$0200
        bsr     le8ae

        ldy     #$027f
        bsr     le8ae

        lda     #$00
        sta     ld14c
        ldy     #l0000
        ldd     #$0280
        lbsr    le8ae

        ldx     #$01ff
        lbsr    le8ae

        lbra    le7d7

LE93F
        ldx     #$d240
        lbsr    lf761

        lda     #$09
        lbsr    ToTaskinA

        lda     #$34
        sta     <l00c2
        lda     #$0a
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     #$09
        lbsr    ToTaskinA

        lbsr    lf7a2

        lda     ld108
        lbsr    lf6ed

        lda     #$ff
        sta     ld148
        sta     ld14a
        sta     ld14c
        ldx     #l0000
        ldy     #l0000
        ldd     #$0100
        lbsr    le8ae

        ldy     #$027f
        lbsr    le8ae

        ldx     #$004d
        ldy     #$0083
        ldd     #$00b4
        lbsr    le8ae

        ldy     #$01fc
        lbsr    le8ae

        lda     #$00
        sta     ld148
        ldx     #l0008
        ldy     #l0008
        ldd     #$00f8
        lbsr    le8ae

        ldy     #$0277
        lbsr    le8ae

        lda     #$00
        sta     ld14a
        ldx     #l0020
        ldy     #$0050
        ldd     #$0031
        lbsr    le8ae

        ldy     #$022f
        lbsr    le8ae

        ldx     #$00cf
        ldd     #$00e0
        lbsr    le8ae

        ldy     #$0050
        lbsr    le8ae

        lda     #$00
        sta     ld14c
        lda     #$ff
        sta     ld148
        sta     ld14a
        ldx     #l0000
        ldy     #l0000
        ldd     #$0280
        lbsr    le8ae

        ldx     #$00ff
        lbsr    le8ae

        lda     #$00
        sta     ld148
        ldx     #l0008
        ldy     #l0008
        ldd     #$0278
        lbsr    le8ae

        ldx     #$00f7
        lbsr    le8ae

        ldx     #l0080
        ldy     #$0050
        ldd     #$00b7
        lbsr    le8ae

        ldy     #$01c9
        ldd     #$0230
        lbsr    le8ae

        lda     #$00
        sta     ld14a
        ldx     #l0020
        ldy     #$0050
        ldd     #$0061
        lbsr    le8ae

        ldx     #$00df
        lbsr    le8ae

        ldx     #l0020
        ldy     #$021f
        ldd     #$0230
        lbsr    le8ae

        ldx     #$00df
        lbsr    le8ae

        ldy     #$010e
LEA57
        ldx     #$0074
LEA5A
        lbsr    le85d

        leax    1,x
        cmpx    #$008c
        blo     lea5a

        leay    1,y
        cmpy    #$012f
        blo     lea57

        ldy     #$012f
LEA70
        ldx     #$0074
LEA73
        lbsr    le84d

        leax    1,x
        cmpx    #$008c
        blo     lea73

        leay    1,y
        cmpy    #$0151
        blo     lea70

        ldy     #$0151
LEA89
        ldx     #$0074
LEA8C
        lbsr    le83f

        leax    1,x
        cmpx    #$008c
        blo     lea8c

        leay    1,y
        cmpy    #$0172
        blo     lea89

        ldy     #$010e
        ldd     #$010f
        lbsr    led0e

        ldy     #$0110
        ldd     #$0112
        lbsr    led0e

        ldy     #$0114
        ldd     #$0117
        lbsr    led0e

        ldy     #$011a
        ldd     #$011e
        lbsr    led0e

        ldy     #$0122
        ldd     #$0127
        lbsr    led0e

        ldy     #$012c
        ldd     #$0132
        lbsr    led0e

        ldy     #$0138
        ldd     #$013f
        lbsr    led0e

        ldy     #$0146
        ldd     #$014e
        lbsr    led0e

        ldy     #$0156
        ldd     #$015f
        lbsr    led0e

        ldy     #$0168
        ldd     #$0172
        lbsr    led0e

        ldx     #$0091
        ldd     #$0092
        lbsr    led27

        ldx     #$0093
        ldd     #$0095
        lbsr    led27

        ldx     #$0097
        ldd     #$009a
        lbsr    led27

        ldx     #$009d
        ldd     #l00a1
        lbsr    led27

        ldx     #$00a5
        ldd     #$00aa
        lbsr    led27

        ldx     #$00af
        ldd     #$00b5
        lbsr    led27

        ldx     #$00bb
        ldd     #l00c2
        lbsr    led27

        ldx     #$00c9
        ldd     #$00d1
        lbsr    led27

        ldx     #$00d9
        ldd     #l00e3
        lbsr    led27

        ldx     #$00ec
        ldd     #$00f6
        lbsr    led27

        lda     #$ff
        sta     ld148
        ldx     #l0080
        ldy     #$0050
        ldd     #$0083
        lbsr    lecd0

        ldx     #l0080
        ldy     #$01c9
        ldd     #$01fc
        lbsr    lecd0

        lda     #$ff
        sta     ld14a
        sta     ld14c
        ldx     #$0030
        ldy     #$021f
        ldd     #$0230
        lbsr    led41

        ldx     #$00df
        ldy     #$0050
        ldd     #$0061
        lbsr    led41

        ldx     #l0080
        ldy     #$004c
        ldd     #$0084
        lbsr    led41

        ldx     #$00b7
        ldy     #$0083
        ldd     #$00bb
        lbsr    led41

        ldx     #l0080
        ldy     #$01c5
        ldd     #$01fc
        lbsr    led41

        ldx     #$00b7
        ldy     #$01fc
        ldd     #$0234
        lbsr    led41

        lda     #$00
        sta     ld14c
        ldx     #l0020
        ldy     #$0050
        ldd     #$0061
        lbsr    led41

        ldx     #$00cf
        ldy     #$021f
        ldd     #$0230
        lbsr    led41

        ldx     #$0049
        ldy     #$0083
        ldd     #$00bb
        lbsr    led41

        ldx     #l0080
        ldy     #$004c
        ldd     #$0084
        lbsr    led41

        ldx     #$0049
        ldy     #$01fc
        ldd     #$0234
        lbsr    led41

        ldx     #l0080
        ldy     #$01c5
        ldd     #$01fd
        lbsr    led41

        lda     #$00
        sta     ld148
        ldx     #l0080
        ldy     #$00b6
        ldd     #$0083
        lbsr    lecd0

        ldx     #l0080
        ldy     #$022f
        ldd     #$01fc
        lbsr    lecd0

        lda     #$ff
        sta     ld14c
        ldx     #l0080
        ldy     #$0048
        ldd     #$0084
        lbsr    led41

        ldx     #$00bb
        ldy     #$0083
        ldd     #$00bf
        lbsr    led41

        ldx     #l0080
        ldy     #$01c1
        ldd     #$01fd
        lbsr    led41

        ldx     #$00bb
        ldy     #$01fc
        ldd     #$0238
        lbsr    led41

        ldd     #l0000
        std     ld14c
        ldx     #$0045
        ldy     #$0083
        ldd     #$00bf
        lbsr    led41

        ldx     #l0080
        ldy     #$0048
        ldd     #$0084
        lbsr    led41

        ldx     #$0045
        ldy     #$01fc
        ldd     #$0238
        lbsr    led41

        ldx     #l0080
        ldy     #$01c1
        ldd     #$01fd
        lbsr    led41

        ldx     #l0080
        ldy     #$0050
        ldd     #$00b7
        lbsr    le8ae

        ldy     #$01c9
        ldd     #$0230
        lbsr    le8ae

        lbra    lf713

LECD0
        std     ld144
        tfr     x,d
        addd    #$0001
LECD8
        bsr     lecf8

        leax    -1,x
        addd    #$0001
        tst     ld148
        bne     lecef

        leay    -1,y
        cmpy    ld144
        bhi     lecd8

        jmp     lecf7

LECEF
        leay    1,y
        cmpy    ld144
        blo     lecd8

LECF7
        rts
LECF8
        pshs    y,x
        pshs    b,a
        std     ld146
LECFF
        lbsr    le83f

        leax    1,x
        cmpx    ld146
        blo     lecff

        puls    b,a
        puls    y,x
        rts
LED0E
        std     ld142
LED11
        ldx     #$000b
LED14
        lbsr    le85d

        leax    1,x
        cmpx    #$006f
        blo     led14

        leay    1,y
        cmpy    ld142
        blo     led11

        rts
LED27
        std     ld140
LED2A
        ldy     #$010e
LED2E
        lbsr    le84d

        leay    1,y
        cmpy    #$0172
        blo     led2e

        leax    1,x
        cmpx    ld140
        blo     led2a

        rts
LED41
        pshs    y,x
        pshs    b,a
        std     ld14e
LED48
        tst     ld14a
        bne     led53

        lbsr    le83f

        jmp     led61

LED53
        tst     ld148
        bne     led5e

        lbsr    le84d

        jmp     led61

LED5E
        lbsr    le85d

LED61
        leay    1,y
        tst     ld14c
        bne     led6d

        leax    1,x
        jmp     led6f

LED6D
        leax    -1,x
LED6F
        cmpy    ld14e
        blo     led48

        puls    b,a
        puls    y,x
        rts
	
LED7A	
        ldx     #$d250
        lbsr    lf761

        clra
        lbsr    ToTaskinA

        lda     <l00c2
        ora     ld600
        sta     <l00c2
        ldy     #l0000
LED8F
        ldb     #$00
        stb     ,y+
        pshs    y
        puls    b,a
        cmpa    #$10
        bne     led8f

        ldx     #leeb1
        ldy     #l0000
        bsr     lee01

        ldx     #$eee4
        ldy     #$05fa
        lbsr    leea6

        ldx     #$eeef
        ldy     #$00d2
        lbsr    leea6

        ldx     #$eeef
        ldy     #$0122
        lbsr    leea6

        ldx     #$ef02
        ldy     #$02b2
        lbsr    leea6

        ldx     #$ef1d
        ldy     #$0352
        lbsr    leea6

        ldx     #$ef38
        ldy     #$03f2
        lbsr    leea6

        ldx     #$ef53
        ldy     #$0492
        lbsr    leea6

        ldx     #$ef6e
        ldy     #$0532
        lbsr    leea6

        ldx     #$ef89
        ldy     #$05d2
        lbsr    leea6

        lbra    lf713

LEE01
        lda     ,x+
        cmpa    #$ff
        beq     lee13

        sta     ,y+
        lda     ,x+
        sta     ,y
        leay    81,y
        jmp     lee01

LEE13
        rts

LEE14	
        ldx     #$d260
        lbsr    lf761

        clra
        lbsr    ToTaskinA

        lda     <l00c2
        ora     ld700
        sta     <l00c2
        ldy     #l0000
LEE29
        ldb     #$00
        stb     ,y+
        pshs    y
        puls    b,a
        cmpa    #$10
        bne     lee29

        ldx     #leeb1
        ldy     #l0000
        bsr     lee92

        ldx     #$eee4
        ldy     #$0b4e
        bsr     leea6

        ldx     #$eeef
        ldy     #$01a4
        bsr     leea6

        ldx     #$eeef
        ldy     #$0244
        bsr     leea6

        ldx     #$ef02
        ldy     #$0604
        bsr     leea6

        ldx     #$ef1d
        ldy     #$0744
        bsr     leea6

        ldx     #$ef38
        ldy     #$0884
        bsr     leea6

        ldx     #$ef53
        ldy     #$09c4
        bsr     leea6

        ldx     #$ef6e
        ldy     #$0b04
        bsr     leea6

        ldx     #$ef89
        ldy     #$0c44
        bsr     leea6

        lbra    lf713

LEE92
        lda     ,x+
        cmpa    #$ff
        beq     leea5

        sta     ,y+
        lda     ,x+
        sta     ,y
        leay    161,y
        jmp     lee92

LEEA5
        rts
LEEA6
        lda     ,x+
        cmpa    #$ff
        beq     leea5

        sta     ,y+
        jmp     leea6

LEEB1
        FCB    $41,$38,$42,$38,$43,$38,$44,$38
        FCB    $45,$38,$46,$38,$47,$38,$48,$38
        FCB    $49,$38,$4A,$38,$4B,$38,$4C,$38
        FCB    $4D,$38,$4E,$38,$4F,$38,$50,$38
        FCB    $51,$38,$52,$38,$53,$38,$54,$38
        FCB    $55,$38,$56,$38,$57,$38,$58,$38
        FCB    $59,$38,$FF,$46,$B8,$4C,$B8,$41
        FCB    $B8,$53,$B8,$48,$B8,$FF,$55,$78
        FCB    $4E,$78,$44,$78,$45,$78,$52,$78
        FCB    $4C,$78,$49,$78,$4E,$78,$45,$78
        FCB    $FF,$52,$0A,$45,$0A,$44,$0A,$20
        FCB    $0A,$4F,$0A,$4E,$0A,$20,$0A,$47
        FCB    $0A,$52,$0A,$45,$0A,$45,$0A,$4E
        FCB    $0A,$20,$0A,$FF,$52,$0C,$45,$0C
        FCB    $44,$0C,$20,$0C,$4F,$0C,$4E,$0C
        FCB    $20,$0C,$42,$0C,$4C,$0C,$55,$0C
        FCB    $45,$0C,$20,$0C,$20,$0C,$FF,$42
        FCB    $22,$4C,$22,$55,$22,$45,$22,$20
        FCB    $22,$4F,$22,$4E,$22,$20,$22,$47
        FCB    $22,$52,$22,$45,$22,$45,$22,$4E
        FCB    $22,$FF,$42,$21,$4C,$21,$55,$21
        FCB    $45,$21,$20,$21,$4F,$21,$4E,$21
        FCB    $20,$21,$52,$21,$45,$21,$44,$21
        FCB    $20,$21,$20,$21,$FF,$47,$14,$52
        FCB    $14,$45,$14,$45,$14,$4E,$14,$20
        FCB    $14,$4F,$14,$4E,$14,$20,$14,$42
        FCB    $14,$4C,$14,$55,$14,$45,$14,$FF
        FCB    $47,$11,$52,$11,$45,$11,$45,$11
        FCB    $4E,$11,$20,$11,$4F,$11,$4E,$11
        FCB    $20,$11,$52,$11,$45,$11,$44,$11
	FCB    $20,$11,$FF

LEFA4
        swi
        lds     #$dfe0
        lbra    le2eb

SWIVector
        lbsr    ClsText40

        ldx     #lf308
        ldb     #$01
        stb     ld001
LEFB7
        ldd     ,x
        beq     lefc0
        lbsr    WriteStrPos

        bra     lefb7

LEFC0
        ldu     #$f5b6
        lda     #$01
        pshs    a
LEFC7
        ldx     ,u++
        beq     lefdc

        lda     ,s
        lda     a,s
        ldb     #$3c
        stb     1,x
        stb     3,x
        lbsr    lf744

        inc     ,s
        bra     lefc7

LEFDC
        leas    1,s
        lda     ,s
        ldx     #$0720
        ldb     #$07
        pshs    b
        ldb     #$30
LEFE9
        stb     ,x++
        dec     ,s
        bmi     leff6

        lsla
        bhs     lefe9

        inc     -2,x
        bra     lefe9

LEFF6
        clr     ld015
        leas    1,s
        ldd     #$06e0
        lbsr    lf1c0

        andcc   #$ef
LF003
        lda     ld015
        lbmi    lf101

        lbsr    lf246

        tstb
        beq     lf018

        lbsr    lf259

        clr     ld015
        bra     lf003

LF018
        ldu     #$f5d0
        pshs    a
        ldb     #$ff
LF01F
        lda     ,u+
        beq     lf033

        incb
        cmpa    ,s
        bne     lf01f

        clr     ld015
        leas    1,s
        lslb
        ldu     #$f5d7
        jmp     [b,u]

LF033
        leas    1,s
        bra     lf003

        ldx     #lf579
LF03A
        ldd     ,x
        beq     lf043

        lbsr    WriteStrPos

        bra     lf03a

LF043
        ldd     #$055a
        tfr     d,x
        lbsr    lf1c0

        lda     #$01
        ldb     #$3c
LF04F
        stb     a,x
        inca
        inca
        cmpa    #$09
        bne     lf04f

LF057
        lda     ld015
        lbsr    lf246

        tstb
        bne     lf068

        cmpa    #$58
        lbeq    lf2b4

        bra     lf057

LF068
        sta     ,x++
        clr     ld015
        cmpx    #$0562
        blo     lf057

        leax    -8,x
        lbsr    lf1e3

        pshs    a
        leax    4,x
        lbsr    lf1e3

        puls    b
        exg     a,b
        andb    #$f0
        tfr     d,y
LF086
        ldx     #$05f2
        lda     #$01
        ldb     #$3c
LF08D
        stb     a,x
        adda    #$02
        cmpa    #$4f
        bne     lf08d

        ldb     #$38
        lda     #$09
LF099
        stb     a,x
        adda    #$0a
        cmpa    #$4f
        bne     lf099

        lda     #$08
        pshs    a
LF0A5
        lda     ,y+
        lbsr    lf744

        leax    4,x
        lda     ,y+
        lbsr    lf744

        leax    6,x
        dec     ,s
        bne     lf0a5

        leas    1,s
        leay    -16,y
        sty     ld000
        lbra    lf003

        ldb     ld001
        bitb    #$01
        lbne    lf003

        lbsr    lf276

        lbsr    lf2bb

        bra     lf086

        ldb     ld001
        bitb    #$01
        lbne    lf003

        lbsr    lf276

        leay    -32,y
        lbsr    lf2bb

        bra     lf086

        bsr     lf0ea

        rti
LF0EA
        ldb     ld001
        bitb    #$01
        bne     lf0f4

        lbsr    lf276

LF0F4
        lbsr    lf29b

        lbsr    ClsText40

        ldd     #$0000
        lbsr    lf1c0

        rts
LF101
        lbsr    lf19d

        lbra    lf003

        ldx     #lf5a9
LF10A
        ldd     ,x
        beq     lf113

        lbsr    WriteStrPos

        bra     lf10a

LF113
        lda     #$3c
        ldx     #$0578
        sta     1,x
        sta     3,x
        sta     23,x
        sta     25,x
        sta     27,x
        sta     29,x
        ldd     #$0578
        lbsr    lf1c0

LF12E
        lda     ld015
        bmi     lf152

        beq     lf12e

        lbsr    lf246

        tstb
        beq     lf143

        lbsr    lf259

        clr     ld015
        bra     lf12e

LF143
        cmpa    #$58
        lbeq    lf2b4

        cmpa    #$0d
        beq     lf156

        clr     ld015
        bra     lf12e

LF152
        bsr     lf19d

        bra     lf12e

LF156
        ldx     #$0578
        bsr     lf1d4

        sta     ld040
        beq     lf162

        bra     lf179

LF162
        ldx     #$058e
        bsr     lf1d4

        sta     ld041
        leax    4,x
        bsr     lf1d4

        sta     ld042
        ldd     ld041
        bne     lf179

        inc     ld040
LF179
        lbsr    lf0ea

        orcc    #$40
        tst     <l0022
        lda     #$3f
        sta     <l0023
LF184
        sync
        orcc    #$10
        lda     <l0023
        bpl     lf184

        tst     <l0022
        lda     #$0c
        ldb     ,s
        andb    #$bf
        stb     ,s
LF195
        deca
        bne     lf195

        nop
        nop
        nop
        rti
        nop
LF19D
        ldu     #$f5e3
        suba    #$80
        cmpa    #$0a
        bhi     lf1ab

        clr     ld015
        jsr     [a,u]

LF1AB
        rts
        ldd     #$0000
        bsr     lf1c0

        rts
LF1B2
        ldd     #$0e0f
        sta     <l0080
        lda     <l0081
        stb     <l0080
        ldb     <l0081
        lslb
        rola
        rts
LF1C0
        lsra
        rorb
        pshs    b,a
        ldd     #$0f0e
        stb     <l0080
        puls    b
        stb     <l0081
        sta     <l0080
        puls    a
        sta     <l0081
        rts
LF1D4
        ldd     #$2030
        cmpa    ,x
        bne     lf1dd

        stb     ,x
LF1DD
        cmpa    2,x
        bne     lf1e3

        stb     2,x
LF1E3
        lda     ,x
        suba    #$30
        cmpa    #$09
        bls     lf1ed

        suba    #$07
LF1ED
        lsla
        lsla
        lsla
        lsla
        ldb     2,x
        subb    #$30
        cmpb    #$09
        bls     lf1fb

        subb    #$07
LF1FB
        pshs    b
        ora     ,s+
        rts
        bsr     lf1b2

        cmpd    #$0050
        bhi     lf20b

        addd    #$07d0
LF20B
        subd    #$0050
        bsr     lf1c0

        rts
        bsr     lf1b2

        cmpd    #$0780
        blo     lf21c

        subd    #$0780
LF21C
        addd    #$0050
        bsr     lf1c0

        rts
        bsr     lf1b2

        cmpd    #$0000
        bne     lf22d

        addd    #$07d0
LF22D
        subd    #$0002
        bsr     lf1c0

        rts
        lbsr    lf1b2

        cmpd    #$07ce
        bne     lf23f

        subd    #$07d0
LF23F
        addd    #$0002
        lbsr    lf1c0

        rts
LF246
        clrb
        cmpa    #$30
        blo     lf258

        cmpa    #$39
        bls     lf257

        cmpa    #$46
        bhi     lf258

        cmpa    #$41
        blo     lf258

LF257
        incb
LF258
        rts
LF259
        pshs    a
        lbsr    lf1b2

        tfr     d,x
        ldb     1,x
        andb    #$07
        cmpb    #$04
        puls    a
        bne     lf272

        sta     ,x++
LF26C
        tfr     x,d
        lbsr    lf1c0

        rts
LF272
        leax    2,x
        bra     lf26c

LF276
        ldy     ld000
        ldx     #$05f2
        lda     #$08
        pshs    a
LF281
        lbsr    lf1e3

        sta     ,y+
        leax    4,x
        lbsr    lf1e3

        sta     ,y+
        leax    6,x
        dec     ,s
        bne     lf281

        leas    1,s
        ldb     #$01
        stb     ld001
        rts
LF29B
        ldu     #$f5b6
        lda     #$05
        pshs    a
LF2A2
        ldx     ,u++
        beq     lf2b1

        lbsr    lf1e3

        ldb     ,s
        sta     b,s
        inc     ,s
        bra     lf2a2

LF2B1
        leas    1,s
        rts
LF2B4
        lds     #$dfe0
        jmp     le2eb

LF2BB
        ldx     #$055a
        tfr     y,d
        lbsr    lf744

        tfr     y,d
        tfr     b,a
        leax    4,x
        lbsr    lf744

        rts
	
FIRQVector
        pshs    b,a
        tst     <l0022
        lda     ld040
        beq     lf2db

        dec     ld040
        beq     lf2f3

LF2DB
        ldd     ld041
        cmpd    3,s
        beq     lf2f3

        sync
        tst     <l0022
        lda     #$10
LF2E8
        deca
        bne     lf2e8

        puls    b,a
        nop
        nop
        brn     lf2e8

        rti
        nop
LF2F3
        lda     2,s
        ora     #$80
        sta     2,s
        ldb     #$3c
        stb     <l0023
        tst     <l0022
        puls    b,a
        puls    cc
        pshs    u,y,x,dp,b,a,cc
        lbra    SWIVector

LF308
        FDB    $0052
        FCN    "D100 =COLOUR RAM 0,2,4 (6=W 8=B BACK)"

        FDB    $00A2
        FCN    "D300 =COLOUR BAR FFFF=VERT 0000=HORIZ"

        FDB    $00F2
        FCN    "D500 =COLOUR BAR COLOURS (TO D51E)"

        FDB    $0142
        FCN    "D600 =PIA DATA FOR LO RES CHARACTER"

        FDB    $0192
        FCN    "D700 =PIA DATA FOR HI RES CHARACTER"

        FDB    $01E2
        FCN    "PIA      0=W1  1=SWCHR  2=HL  3=CG"

        FDB    $0232
        FCN    "BITS     4=CONTROL  5=FS  6=PG0  7=PG1"

        FDB    $028C
        FCN    "6845 SET UP DATA (16 REGS.)"

        FDB    $02D2
        FCN    "D200 =LO RES  4 COLR 320.256 TESTS 0,1"

        FDB    $0322
        FCN    "D210 =LO RES 16 COLR 320.256 TEST  2"

        FDB    $0372
        FCN    "D220 =HI RES  2 COLR 640.512 TESTS 3,4"

        FDB    $03C2
        FCN    "D230 =HI RES  4 COLR 640.512 TESTS 5,6"

        FDB    $0412
        FCN    "D240 =HI RES  4 COLR 640.256 TEST  7"
	
        FDB    $0462
        FCN    "D250 =LO RES CHAR. 40 COLUMN TEST  8"

        FDB    $04B2
        FCN    "D260 =HI RES CHAR. 80 COLUMN TEST  9"

        FDB    $0690
        FCN    	"CC A  B  DP X    Y    U    PC   EFHINZVC"
	
        FDB    	$0000

LF579        
	FDB    	$0550
	FCN	"ADR."

        FDB    $05A2
        FCN    "0    2    4    6    8    A    C    E"

        FDB    $0000

LF5A9
        FDB    $0570
        FCN    "NO."

        FDB    $0586
	FCN    "PC"
	
        FDB    $0000

        FCB    $06,$E0,$06,$E6,$06,$EC,$06,$F2
        FCB    $06,$F8,$06,$FC,$07,$02,$07,$06
        FCB    $07,$0C,$07,$10,$07,$16,$07,$1A
        FCB    $00,$00,$52,$54,$4D,$58,$48,$4C
        FCB    $00,$F0,$E7,$F1,$07,$F0,$37,$F2
        FCB    $B4,$F0,$C2,$F0,$D3,$F2,$00,$F2
        FCB    $11,$F2,$22,$F2,$33,$F1,$AC

IRQVector
        lda     <(A.GCon-IOBase)	; read control register of graphics port PIA
        bita    #VSyncFlag		; Are we in vsync?
        bne     lf602			; yes, deal with it

; Clear other interrupts

        lda     <(DAT.Task-IOBase)
        lda     <(A.GCon-IOBase)+2
        lda     <(A.P-IOBase)
        lda     <(A.Kbd-IOBase)
        lda     <(A.Mouse-IOBase)
        lda     <(A.Mouse-IOBase)+2
        sta     <(A.T1-IOBase)+1
        rti				; return from interrupt
	
LF602
        lda     ld00a
        bpl     lf60a

        dec     ld00a
LF60A
        lda     <(A.GCon-IOBase)	; clear int
        lda     <(A.Kbd-IOBase)		; read keyboard
        bita    #KAnyKey		; Any key pressed ?
        bne     lf619			; yes, deal with it

        clr     ld018			; clear some vars
        clr     ld017
        rti
	
LF619
        tst     ld017			; debounce ?
        bne     lf622

        com     ld017
        rti

LF622
        ldx     #KBLookup		; point to keyboard lookup table
        ldb     #$0a			; 10 columns
        lda     <(A.Kbd-IOBase)
        ora     #KClkDataIn		; Data in high
        sta     <(A.Kbd-IOBase)

; Clock data in to all the keyboard flipflops

LF62D
        lda     <(A.Kbd-IOBase)+1	; read control register
        anda    #$ff-KInLoad		; data in clock low
        sta     <(A.Kbd-IOBase)+1	
        ora     #KInLoad		; data in clock high	
        sta     <(A.Kbd-IOBase)+1
        decb				; dec clockpulse counter
        bne     lf62d			; loop again if not all done

        lda     <(A.Kbd-IOBase)		; set output data high
        anda    #$ff-KClkDataIn
        sta     <(A.Kbd-IOBase)
	
; clock a walking 0 through the 10 columns of the keyboard.
; checking for any key pressed after each column, if a key is pressed
; clock it in, otherwise move to next column	
	
        lda     <(A.Kbd-IOBase)+1	; Clock out low bit
        anda    #$ff-KInLoad		; clock low
        sta     <(A.Kbd-IOBase)+1
        ora     #$KInLoad		; clock high
        sta     <(A.Kbd-IOBase)+1
	
        ldb     #$0a			; 10 columns
LF64C
        lda     <(A.Kbd-IOBase)		; read keyboard
        bita    #KAnyKey		; any key pressed in this row
        bne     lf663			; yes : deal with it

; process next column
        leax    7,x			; increment key table pointer to next col
        lda     <(A.Kbd-IOBase)+1	; clock out for next row
        anda    #$ff-KInLoad		; clock low
        sta     <(A.Kbd-IOBase)+1
        ora     #$KInLoad		; clock high
        sta     <(A.Kbd-IOBase)+1
	
        decb				; decrement col counter
        bne     lf64c			; no zero, do next

        bra     lf6c2			; finished, skip ahead.

; This row has key(s) pressed, clock them in

LF663
        ldb     #$08			; 8 rows

; Move to next row, this clocks current row into input shift register	
        lda     <(A.Kbd-IOBase)+1	; get Kbd ctrl register	
        anda    #$ff-KInLoad		; clock low
        sta     <(A.Kbd-IOBase)+1
        ora     #$KInLoad		; clock high
        sta     <(A.Kbd-IOBase)+1
	
        lda     <(A.Kbd-IOBase)		; get keyboard IO register
LF671
        anda    #$FF-KClkDataIn		
        sta     <(A.Kbd-IOBase)
        ora     #KClkDataIn
        sta     <(A.Kbd-IOBase)
        decb
        beq     lf6c2

        lda     <(A.Kbd-IOBase)
        bita    #$20
        bne     lf686

        leax    1,x
        bra     lf671

LF686
        lda     ,x
        stx     ld002
        ldb     ld014
        bitb    #$40
        beq     lf697

        leax    70,x
        lda     ,x
LF697
        ldb     ld018
        bpl     lf6ac

        cmpb    #$99
        beq     lf6a5

        inc     ld018
        bra     lf6c2

LF6A5
        subb    #$03
        stb     ld018
        bra     lf6b1

LF6AC
        ldb     #$80
        stb     ld018
LF6B1
        cmpa    #$40
        beq     lf6ba

        sta     ld015
        bra     lf6c2

LF6BA
        ldb     ld014
        eorb    #$40
        stb     ld014
LF6C2
        lda     <l0022
        anda    #$ef
        sta     <l0022
        ldb     #$0a
LF6CA
        lda     <l0023
        anda    #$f7
        sta     <l0023
        ora     #$08
        sta     <l0023
        decb
        bne     lf6ca

        anda    #$f7
        sta     <l0023
        lda     <l0022
        ldb     ld014
        bitb    #$40
        bne     lf6e8

        anda    #$ef
        bra     lf6ea

LF6E8
        ora     #$10
LF6EA
        sta     <l0022
        rti
LF6ED
        sta     <l00a0
        lda     ld100
        sta     <l00a1
        lda     ld102
        sta     <l00a2
        lda     ld104
        sta     <l00a3
        rts
        lda     ld015
        cmpa    #$58
        bne     lf708

        leas    2,s
LF708
        rts

; Short delay loop
; I think this is supposed to be 4 * 65536, but though b is loaded, it's not 
; decremented or checked, so ends up being 1x65536

ShortDelay
        ldx     #$ffff			; delay 
        ldb     #$04			
LF70E
        leax    -1,x			; decrement
        bne     lf70e			; loop if not zero

        rts				; return
	
LF713
        clra
        lbsr    ToTaskinA

        andcc   #$ef
LF719
        lda     ld015
        cmpa    #$58
        bne     lf719

        lds     #$dfe0
        ldx     #LE050
        bsr     lf761

        lbra    le2eb

LF72C
        ldy     ,u++
        beq     lf738

        ldx     ,u++
        lbsr    lf7bf

        bra     lf72c

LF738
        rts
LF739
        ldx     ,u++
        beq     lf743

        lda     [,u++]
        bsr     lf744

        bra     lf739

LF743
        rts
LF744
        tfr     a,b
        anda    #$0f
        bsr     lf757

        exg     a,b
        lsra
        lsra
        lsra
        lsra
        bsr     lf757

        sta     ,x
        stb     2,x
        rts
LF757
        cmpa    #$09
        bhi     lf75e

        ora     #$30
        rts
LF75E
        adda    #$37
        rts
LF761
        ldb     #$0f
LF763
        lda     ,x+
        stb     <l0080
        sta     <l0081
        decb
        bge     lf763

        rts
        lda     #$00
        bsr     ToTaskinA

        bsr     ClsText40

LF773
        ldy     ,u++
        beq     lf77e

        ldx     ,u++
        bsr     lf7bf

        bra     lf773

LF77E
        rts
LF77F
        ldb     <l0022
        andb    #$83
        bne     lf77f

        ldb     #$35
        sta     <l0020
        stb     <l0021
        eorb    #$08
        nop
        stb     <l0021
LF790
        tst     <l0021
        bpl     lf790

        tst     <l0020
        rts

;
; Change to the task in the A register.
;

ToTaskinA
        pshs    a			; stack task to change to
        lda     <(DAT.Task-IOBase)	; get task register
        anda    #$f0			; mask out task bits
        ora     ,s+			; or in new task from stack
        sta     <(DAT.Task-IOBase)	; set it
        rts

LF7A2
        pshs    y,x,a
        ldx     #$0000
        clra
LF7A8
        sta     ,x+
        cmpx    #$a000
        blo     lf7a8

        puls    pc,y,x,a                ; Pull of PC, effective RTS
LF7B1
        leay    -1,y
        bne     lf7b1

        deca
        bne     lf7b1

        decb
        bne     lf7b1

        rts


WriteStrPos
        ldy     ,x++			; get screen pointer
LF7BF
        lda     ,x+			; get a byte from the string
        beq     lf7c7			; zero = end of string?

        sta     ,y++			; store char on screen, skip over attribute
        bra     lf7bf			; loop for next

LF7C7   rts				; restore and return
	
ClsText40
        pshs    x			; save x
        lda     #$20			; space
        ldb     #$38			; attributes?
        ldx     #$0000			; base of screen ?
LF7D1
        std     ,x++			; save char / attr pair
        cmpx    #$07d2			; past end of screen?
        bne     lf7d1			; no, loop again	

        lda     #$08			; set screen mode
        sta     <(A.GCon-IOBase)
        puls    pc,x                    ; restore & return


; I suspect this is scanning a group of options.....

LF7DE   ldb     ,x+			; get something from mem pointed to (number of keys to scan for?)
        clr     ld015			; clear flag (last key?)

LF7E3   lda     ld015			; read flag (must be changed in IRQ?)
        beq     lf7e3			; still zero loop again

        clr     ld015			; clear flag
	
LF7EB
        cmpa    ,x+			; compare key?
        beq     lf7f6			; equal, exit 

        decb				; decrement count
        beq     lf800			; if not more, exit with error

        leax    2,x			; skip over address?
        bra     lf7eb			; loop for next

LF7F6
        tfr     a,b			; copy key to b
        subb    #$30			; make zero based?
        stb     ld044			; save it
	
        andcc   #$FF-Carry		; flag ok
        rts
	
LF800
        orcc    #Carry			; flag error
        rts
	
LF803
        FCB    $80,$40,$20,$10,$08,$04,$02,$01
        FCB    $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
        FCB    $80,$00,$40,$00,$20,$00,$10,$00
        FCB    $08,$00,$04,$00,$02,$00,$01,$00
        FCB    $00,$80,$00,$40,$00,$20,$00,$10
        FCB    $00,$08,$00,$04,$00,$02,$00,$01

Page1Text
        FDB    $006E
        FCN    "TEST MENU"

        FDB    $00F2
        FCN    "U =MONITOR"

        FDB    $0192
        FCN    "0 =LO RES  4 COLR (WHITE,R1,C1)   AxB"

        FDB    $01E2
        FCN    "1 =LO RES  4 COLR (BLACK,R0,C0)   AxB"

        FDB    $0232
        FCN    "2 =LO RES 16 COLR (COLOUR-BAR )   AxB"

        FDB    $0282
        FCN    "3 =HI RES  2 COLR (WHITE,R1,C1)   CxD"

        FDB    $02D2
        FCN    "4 =HI RES  2 COLR (BLACK,R0,C0)   CxD"

        FDB    $0322
        FCN    "5 =HI RES  4 COLR (WHITE,R1,C1)   CxD"

        FDB    $0372
        FCN    "6 =HI RES  4 COLR (BLACK,R0,C0)   CxD"

        FDB    $03C2
        FCN    "7 =HI RES  4 COLR ( TEST CARD )   CxB"

        FDB    $0462
        FCN    "8 =LO RES 40 COLUMN CHARACTERS "

        FDB    $04B2
        FCN    "9 =HI RES 80 COLUMN CHARACTERS"

        FDB    $06EE
        FCN    "A=320  B=256  C=640  D=512"
	
        FDB    $0000

MessError
        FDB    $0734
        FCN    "ERROR"

MessSpaces
        FDB    $0734
        FCN    "     "

        FDB    $0294
        FCN    "SORRY, TEST NOT WRITTEN YET.          PRESS X"

KeyTable1
        FCB    $0B

        FCB    $55

        FDB    LEFA4

        FCB    $30

        FDB    LE351

        FCB    $31

        FDB    LE499

        FCB    $32

        FDB    LE4F8

        FCB    $33

        FDB    LE5D0

        FCB    $34

        FDB    LE710

        FCB    $35

        FDB    LE77E

        FCB    $36

        FDB    LE8E5

        FCB    $37

        FDB    LE93F

        FCB    $38

        FDB    LED7A

        FCB    $39

        FDB    LEE14

; Keyboard lookup table, translates Matrix -> Key?

KBLookup
        FCB    $00,$40,$06,$38,$00,$00,$00,$37
        FCB    $48,$55,$39,$49,$4A,$42,$36,$47
        FCB    $59,$30,$4F,$4B,$4E,$35,$46,$54
        FCB    $2D,$50,$4C,$4D,$34,$44,$56,$5E
        FCB    $40,$3B,$2C,$33,$53,$52,$5B,$5C
        FCB    $3A,$2E,$20,$58,$43,$5D,$7F,$0D
        FCB    $2F,$32,$5A,$45,$31,$84,$37,$2A
        FCB    $31,$41,$57,$80,$88,$82,$30,$2E
        FCB    $00,$51,$33,$86,$39,$23,$00,$40
        FCB    $06,$28,$00,$00,$00,$27,$68,$75
        FCB    $29,$69,$6A,$62,$26,$67,$79,$30
        FCB    $6F,$6B,$6E,$25,$66,$74,$3D,$70
        FCB    $6C,$6D,$24,$64,$76,$7E,$5C,$2B
        FCB    $3C,$23,$73,$72,$7B,$7C,$2A,$3E
        FCB    $20,$78,$63,$7D,$7F,$0D,$3F,$32
        FCB    $7A,$65,$31,$34,$37,$2A,$21,$61
        FCB    $77,$32,$35,$38,$30,$2E,$00,$71
        FCB    $33,$36,$39,$23,$0D,$02,$07,$39
        FCB    $12,$14,$19,$1B,$2A,$2C,$00,$3F
        FCB    $00,$41,$00,$31,$00,$33

SWI2Vector
        ldd     #$0000
        std     ldffe
        jmp     [ldffe]

        FCB    $00

        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00

        FDB    SWI2Vector		; SWI3
        FDB    SWI2Vector		; SWI2
        FDB    FIRQVector		; FIRQ
        FDB    IRQVector		; IRQ
        FDB    SWIVector		; SWI
        FDB    ResetVector		; NMI
        FDB    ResetVector		; Reset
