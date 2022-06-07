* Disassembled by BeebDIS/6809, V1.30, at 11:30 2022-05-24

                use     defsfile

tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     1

first           mod     eom,name,tylg,atrv,start,size

; 3103

u0000           rmb     1
u0001           rmb     1
u0002           rmb     1
u0003           rmb     1
u0004           rmb     1
u0005           rmb     1
u0006           rmb     1
u0007           rmb     1
u0008           rmb     2
u000A           rmb     1
u000B           rmb     1
u000C           rmb     2
u000E           rmb     2
u0010           rmb     2
u0012           rmb     2
u0014           rmb     2
u0016           rmb     2
u0018           rmb     2
u001A           rmb     2
u001C           rmb     2
u001E           rmb     2
u0020           rmb     1
u0021           rmb     2
u0023           rmb     1
u0024           rmb     2
u0026           rmb     1
u0027           rmb     2
u0029           rmb     2
u002B           rmb     2
u002D           rmb     2
u002F           rmb     1
u0030           rmb     2
u0032           rmb     4
u0036           rmb     1
u0037           rmb     1
u0038           rmb     1
u0039           rmb     1
u003A           rmb     1
u003B           rmb     1
u003C           rmb     1
u003D           rmb     1
u003E           rmb     1
u003F           rmb     1
u0040           rmb     1
u0041           rmb     1
u0042           rmb     1
u0043           rmb     2
u0045           rmb     2
u0047           rmb     2
u0049           rmb     2
u004B           rmb     2
u004D           rmb     2
u004F           rmb     2
u0051           rmb     19
u0064           rmb     7
u006B           rmb     2
u006D           rmb     2
u006F           rmb     26
u0089           rmb     68
u00CD           rmb     2
u00CF           rmb     2
u00D1           rmb     2
u00D3           rmb     2
u00D5           rmb     19
u00E8           rmb     4
u00EC           rmb     11
u00F7           rmb     7
u00FE           rmb     121
u0177           rmb     255
u0276           rmb     40
u029E           rmb     384
u041E           rmb     2049
size            equ     .


name            FCS     "Edit"          ; OS9 Module name

                FCB     $03

                FCC     "(C)1981Microware"

L0022           FCB        $01    ; PC=8022 INVALID opcode 01

                lbra    L0292

                lbra    L0C38

                lbra    L0BEC

                lbra    L0BE3

                lbra    L100F

                lbra    L0770

                lbra    L06EA

                lbra    L0716

                lbra    L073F

                lbra    L04A1

                lbra    L046B

                lbra    L05D6

                lbra    L0626

                lbra    L0094

                lbra    L0B96

                lbra    L0B01

L0053           lda     #$00
                sta     <u0023,U
                rti
L0059           ldx     <u000E
                jmp     ,X

L005D           lda     <u0026
                lbsr    L0107

                lda     #$3A
                lbsr    L0107

                ldx     <u0012
                leax    375,X
                lda     <u000A
                ldy     #$0080
                os9     I$ReadLn
                bhs     L0081

                cmpb    #$D3
                lbeq    L114D

                lbra    L12AA

L0081           rts
L0082           pshs    X,B,A
                lbsr    L06D7

                bsr     L008B

                puls    PC,X,B,A        ; Pull of PC, effective RTS
L008B           pshs    B,A
                lbsr    L0130

                bsr     L0094

                puls    PC,B,A          ; Pull of PC, effective RTS
L0094           pshs    Y,X,B,A
                cmpd    #$0000
                beq     L00C6

                leay    D,X
                pshs    Y
L00A0           tst     <u0037
                beq     L00AC

                lda     #$20
                bsr     L0107

                bsr     L0107

                bsr     L0107

L00AC           tst     <u0023
                beq     L00C4

                lda     ,X+
                bsr     L0107

                cmpx    ,S
                beq     L00BE

                cmpa    #$0D
                bne     L00AC

                bra     L00A0

L00BE           cmpa    #$0D
                beq     L00C4

                bsr     L00FF

L00C4           puls    Y
L00C6           puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L00C8           pshs    Y,B,A
                ldd     <u0002
                tstb
                beq     L00E0

                cmpa    ,S
                bne     L00E0

                ldd     <u0021
                addd    #$0001
                std     <u0021
                ldb     <u0020
                adcb    #$00
                stb     <u0020
L00E0           bsr     L0130

                tfr     D,Y
                lda     ,S
                os9     I$Write
                lblo    L12AA

                puls    PC,Y,B,A        ; Pull of PC, effective RTS
L00EF           pshs    A
                bsr     L00FF

                lda     <u0037
                clr     <u0037
                bsr     L008B

                sta     <u0037
                puls    PC,A            ; Pull of PC, effective RTS
L00FD           bsr     L00FF

L00FF           pshs    A
                lda     #$0D
                bsr     L0107

                puls    PC,A            ; Pull of PC, effective RTS
L0107           pshs    Y,X,A
                lda     <u000B
                ldy     #$0001
                tfr     S,X
                tst     <u0041
                bmi     L0119

                tst     <u003E
                beq     L0120

L0119           os9     I$WritLn
                lblo    L12AA

L0120           puls    PC,Y,X,A        ; Pull of PC, effective RTS
L0122           pshs    X
                lda     ,X+
                cmpa    #$0D
                lbeq    L1272

                bsr     L0132

                puls    PC,X            ; Pull of PC, effective RTS
L0130           lda     #$0D
L0132           pshs    Y,X
                ldb     #$0D
                ldy     #$0000
L013A           cmpx    <u001C
                beq     L014C

                leay    1,Y
                cmpb    ,X
                beq     L014C

                cmpa    ,X+
                bne     L013A

                leay    -1,Y
                bra     L0152

L014C           cmpa    #$0D
                lbne    L1272

L0152           tfr     Y,D
                cmpd    #$0000
                puls    PC,Y,X          ; Pull of PC, effective RTS
L015A           pshs    U,X,B,A
                os9     F$PrsNam
                puls    PC,U,X,B,A      ; Pull of PC, effective RTS
start           tfr     U,D
                std     <u0012
                sts     <u001E
                leas    u041E,U
                addd    #$041F
                std     <u0014
                std     <u0016
                std     <u001C
                std     <u001A
                pshs    U,Y,X,B,A
                leax    L0022,PCR
                stx     <u000C
                leax    L005D,PCR
                stx     <u000E
                leax    L09DD,PCR
                stx     <u0010
                ldd     #$0000
                std     <u0051
                std     <u006F
                std     <u0089
                leax    L1482,PCR
                stx     <u004D
                leax    L13CF,PCR
                stx     <u006B
                leax    first,PCR
                stx     <u004F
                stx     <u006D
                leax    L13B0,PCR
                lda     #$01
                os9     F$Link
                blo     L01B5

                jsr     ,Y

L01B5           leax    L13B6,PCR
                lda     #$01
                os9     F$Link
                blo     L01C2

                jsr     ,Y

L01C2           puls    U,Y,X,B,A
                lda     #$FF
                sta     <u0041
                inca
                sta     <u0001
                sta     <u0003
                sta     <u0005
                sta     <u0007
                sta     <u000A
                sta     <u003D
                inca
                sta     <u000B
                sta     <u003E
                sta     <u0040
                lda     #$45
                sta     <u0026
                clr     <u0036
                lbsr    L0691

                cmpa    #$0D
                beq     L0260

                lbsr    L015A

                blo     L0259

                lda     #$01
                stx     <u0032
                os9     I$Open
                blo     L024B

                ldb     #$01
                std     <u0000
                pshs    X
                leay    u00F7,U
                leax    L139F,PCR
                ldd     #$0007
                lbsr    L0B96

                pshs    Y
                ldx     <u0032
L020F           cmpx    2,S
                beq     L0220

                lda     ,X+
                sta     ,Y+
                cmpa    #$2F
                bne     L020F

                sty     ,S
                bra     L020F

L0220           puls    Y
                leax    L13A7,PCR
                ldd     #$0008
                lbsr    L0B96

                ldx     <u0032
                ldd     ,S
                subd    <u0032
                sty     <u0032
                lbsr    L0B96

                lda     #$0D
                sta     ,Y
                puls    X
                lbsr    L0691

                cmpa    #$0D
                bne     L024B

                leax    u00FE,U
                inc     <u0036
L024B           ldd     #$020B
                os9     I$Create
                blo     L025B

                ldb     #$02
                std     <u0002
                bra     L0260

L0259           ldb     #$D8
L025B           orcc    #$01
                os9     F$Exit
L0260           ldy     #$0000
                sty     <u0024
                lda     #$42
                lbsr    L06EA

                lbsr    L073F

                lda     #$42
                ldy     #$0001
                lbsr    L06EA

                leax    L0053,PCR
                ldu     <u0012
                os9     F$Icpt
                tst     <u0001
                beq     L028F

                ldd     <u001E
                subd    <u001C
                subd    #$0400
                lbsr    L0F43

L028F           lbsr    L00FD

L0292           ldu     <u0012
                leas    u041E,U
                leax    u029E,U
                stx     <u0045
                stx     <u0047
                leax    <-40,X
                stx     <u0049
                lda     #$FF
                sta     <u0041
                sta     <u0023
                lda     <u0040
                sta     <u003E
                clr     <u003B
                clr     <u003A
                clr     <u003C
                lda     #$01
                sta     <u0037
                tst     <u003D
                beq     L02CB

                tst     <u0039
                bne     L02CB

                leax    L1398,PCR
                lbsr    L00EF

                lbsr    L00FF

L02CB           clr     <u003D
                clr     <u0039
                lbsr    L0059

                leax    u0177,U
                lbsr    L0130

                leay    D,X
                sty     <u002B
                ldy     #$0000
                sty     <u0029
                leau    u0276,U
                stu     <u0027
                lda     ,X
                cmpa    #$20
                bne     L02F8

                leax    1,X
                lbsr    L0BDE

                bra     L0292

L02F8           cmpa    #$0D
                bne     L0308

                ldx     <u001A
                lbsr    L06B2

                stx     <u001A
                lbsr    L008B

                bra     L0292

L0308           bsr     L0310

                lbsr    L00FF

                lbra    L0292

L0310           ldd     <u0043
                pshs    B,A
                ldd     <u0045
                std     <u0043
                pshs    B,A
                lda     <u003E
                pshs    A
                clr     <u002F
                inc     <u0041
L0322           cmpx    <u002B
                lbhs    L03B8

                lbsr    L0691

                cmpa    #$0D
                bne     L0335

                leax    1,X
                clr     <u002F
                bra     L0322

L0335           ldd     <u0027
                pshs    B,A
                stu     <u0027
                ldd     <u0029
                pshs    B,A
                ldd     <u002B
                pshs    B,A
                pshs    U
                lbsr    L0691

                sta     <u0038
                lbsr    L03D3

                pshs    X
                pshs    U
                leax    <L039D,PCR
                pshu    X
                pshu    S
                tfr     D,X
                lda     <u0038
                tst     <u003B
                bne     L0372

                tst     <u003C
                bne     L0372

                tst     <u003D
                beq     L0397

                tst     <u003A
                bne     L0372

                cmpa    #$3A
                bne     L0372

                stb     <u003D
L0372           cmpa    #$5B
                bne     L0378

                inc     <u003A
L0378           cmpa    #$5D
                bne     L039D

                dec     <u003A
                bpl     L039D

                tst     <u003C
                bne     L0395

                lbsr    L0964

                tst     <u003B
                bne     L038F

                clr     <u003D
                bra     L039D

L038F           clr     <u003B
                bra     L039D

                bra     L039D

L0395           clr     <u003C
L0397           lda     <u0041
                clr     <u0039
                jsr     ,Y

L039D           puls    U
                puls    X
                puls    U
                puls    B,A
                std     <u002B
                puls    B,A
                std     <u0029
                puls    B,A
                std     <u0027
                tst     <u0023
                lbeq    L127B

                lbra    L0322

L03B8           dec     <u0041
                puls    A
                sta     <u003E
                puls    B,A
                std     <u0045
                puls    B,A
                std     <u0043
                tst     <u003D
                beq     L03D2

                lda     #$01
                sta     <u003D
                clr     <u003C
                clr     <u003B
L03D2           rts
L03D3           lbsr    L0691

                bsr     L040E

                beq     L0400

                leax    1,X
                lbsr    L04A1

                lbeq    L1269

                pshs    Y
                tfr     D,Y
                ldd     ,Y
                leay    D,Y
                sty     <u002B
                ldy     ,S
                lbsr    L04F4

                tfr     Y,D
                puls    Y
                sty     <u0029
                leay    L0310,PCR
                rts
L0400           pshs    B,A
                lbsr    L04F4

                ldd     #$0000
                std     <u0029
                std     <u002B
                puls    PC,Y            ; Pull of PC, effective RTS
L040E           ldb     ,X+
                lbsr    L04D0

                tfr     A,B
L0415           ldy     <u0012
                leay    <77,Y
                cmpb    #$2E
                bne     L0425

                ldy     <u0012
                leay    <107,Y
L0425           sty     <u004B
                ldy     ,Y
L042B           lda     ,Y
                bne     L043C

                ldy     <u004B
                leay    4,Y
                sty     <u004B
                ldy     ,Y
                beq     L045D

L043C           cmpb    #$2E
                beq     L0446

                cmpb    ,Y+
                bne     L044A

                bra     L044E

L0446           bsr     L046B

                beq     L044E

L044A           bsr     L0462

                bra     L042B

L044E           pshs    Y
                bsr     L0462

                ldd     -2,Y
                ldy     <u004B
                addd    2,Y
                orcc    #$04
                puls    PC,Y            ; Pull of PC, effective RTS
L045D           leax    -1,X
                andcc   #$FB
                rts
L0462           lda     ,Y+
                cmpa    #$0D
                bne     L0462

                leay    2,Y
                rts
L046B           pshs    Y,X,B,A
                lda     ,Y
                bsr     L04DA

                bne     L049D

L0473           sty     4,S
                lda     ,Y+
                bsr     L04D0

                bne     L0491

                pshs    A
                lda     ,X+
                bsr     L04D0

                cmpa    ,S+
                beq     L0473

L0486           sty     4,S
                lda     ,Y+
                bsr     L04DA

                beq     L0486

                bra     L049D

L0491           lda     ,X
                bsr     L04DA

                beq     L049D

                stx     2,S
                orcc    #$04
                bra     L049F

L049D           andcc   #$FB
L049F           puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L04A1           pshs    U
                ldu     <u0014
                lbsr    L0691

L04A8           lda     u0004,U
                cmpa    #$4D
                bne     L04C0

                leay    u000B,U
                lbsr    L069A

                bsr     L046B

                beq     L04C4

                ldd     ,U
                leau    D,U
                cmpu    <u001C
                blo     L04A8

L04C0           orcc    #$04
                puls    PC,U            ; Pull of PC, effective RTS
L04C4           lbsr    L069A

                lbsr    L0691

                tfr     U,D
                andcc   #$FB
                puls    PC,U            ; Pull of PC, effective RTS
L04D0           cmpa    #$61
                blo     L04DA

                cmpa    #$7A
                bhi     L04DA

                suba    #$20
L04DA           cmpa    #$5F
                beq     L04EE

                cmpa    #$41
                blo     L04F1

                cmpa    #$5A
                bls     L04EE

                cmpa    #$61
                blo     L04F1

                cmpa    #$7A
                bhi     L04F1

L04EE           orcc    #$04
                rts
L04F1           andcc   #$FB
                rts
L04F4           clr     <u0042
                pshs    B,A
L04F8           lbsr    L069A

                lbsr    L0691

                bsr     L052B

L0500           cmpa    #$0D
                beq     L0525

                cmpa    #$4C
                bne     L050F

                pshu    X
                lbsr    L06C2

                bra     L0525

L050F           cmpa    #$23
                bne     L0517

                bsr     L0540

                bra     L04F8

L0517           cmpa    #$24
                lbne    L1287

L051D           bsr     L0553

                cmpa    #$24
                beq     L051D

                bra     L0500

L0525           ldb     <u0042
                pshu    B
                puls    PC,B,A          ; Pull of PC, effective RTS
L052B           lbsr    L069A

                pshs    A
                cmpa    #$0D
                beq     L053E

                inc     <u0042
L0536           leay    1,Y
                lda     ,Y
                bsr     L04DA

                beq     L0536

L053E           puls    PC,A            ; Pull of PC, effective RTS
L0540           pshs    B,A
                lda     ,X
                cmpa    #$23
                bne     L054C

                bsr     L05B3

                bra     L0551

L054C           lbsr    L05D6

                pshu    B,A
L0551           puls    PC,B,A          ; Pull of PC, effective RTS
L0553           pshs    B
                lbsr    L0691

                cmpa    #$24
                bne     L0562

                bsr     L05B3

                bsr     L052B

                bra     L057E

L0562           pshu    X
                lbsr    L0122

                leax    D,X
                leax    2,X
                bsr     L052B

                cmpa    #$24
                bne     L057E

                pshs    X,A
                lbsr    L0691

                cmpa    #$24
                puls    X,A
                beq     L057E

                leax    -1,X
L057E           puls    PC,B            ; Pull of PC, effective RTS
L0580           pshs    Y,X,A
                ldy     <u0029
                ldb     #$00
L0587           lbsr    L069A

                cmpa    #$0D
                beq     L05AB

                lbsr    L0691

                addb    #$01
                lda     ,X+
                cmpa    ,Y+
                bne     L059E

                lbsr    L046B

                beq     L05AD

L059E           ldx     1,S
L05A0           lda     ,Y+
                lbsr    L04DA

                beq     L05A0

                leay    -1,Y
                bra     L0587

L05AB           ldb     #$00
L05AD           stx     1,S
                cmpb    #$00
                puls    PC,Y,X,A        ; Pull of PC, effective RTS
L05B3           pshs    Y
                lda     ,X
                cmpa    #$24
                beq     L05C1

                cmpa    #$23
                lbne    L1287

L05C1           bsr     L0580

                lbeq    L1290

                ldy     <u0027
                negb
                addb    4,Y
                lslb
                leay    5,Y
                ldd     B,Y
                pshu    B,A
                puls    PC,Y            ; Pull of PC, effective RTS
L05D6           lda     ,X
                cmpa    #$2A
                bne     L05E3

                leax    1,X
                ldd     #$FFFF
                bra     L05F3

L05E3           ldd     #$0000
                bsr     L0600

                bne     L05EF

                ldd     #$0001
                bra     L05F3

L05EF           bsr     L0600

                bne     L05EF

L05F3           rts
L05F4           beq     L0606

                com     <u00E8
                neg     <u0064
                neg     <u000A
                neg     <u0001
                neg     <u0000
L0600           pshs    Y,B,A
                ldb     ,X
                subb    #$30
L0606           cmpb    #$0A
                bhs     L0622

                leax    1,X
                lda     #$00
                ldy     #$000A
L0612           addd    ,S
                lblo    L1266

                leay    -1,Y
                bne     L0612

                std     ,S
                andcc   #$FB
                puls    PC,Y,B,A        ; Pull of PC, effective RTS
L0622           orcc    #$04
                puls    PC,Y,B,A        ; Pull of PC, effective RTS
L0626           pshs    Y,X,B,A
                leax    >L05F4,PCR
                ldy     #$2F20
L0630           leay    256,Y
                subd    ,X
                bhs     L0630

                addd    ,X++
                pshs    B,A
                ldd     ,X
                tfr     Y,D
                beq     L0659

                ldy     #$2F30
                cmpd    #$3020
                bne     L0652

                ldy     #$2F20
                tfr     B,A
L0652           lbsr    L0107

                puls    B,A
                bra     L0630

L0659           lbsr    L0107

                leas    2,S
                puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L0660           pshs    X,B,A
                ldx     <u0027
                ldd     -2,X
                puls    PC,X,B,A        ; Pull of PC, effective RTS
L0668           pshs    X,B,A
                tst     <u0023
                beq     L0683

                ldx     <u0027
                ldd     -2,X
                beq     L0683

                cmpd    #$FFFF
                bne     L067E

                andcc   #$FB
                bra     L0683

L067E           subd    #$0001
                std     -2,X
L0683           puls    PC,X,B,A        ; Pull of PC, effective RTS
L0685           pshs    X,B,A
                ldx     <u0027
                ldd     -2,X
                cmpd    #$FFFF
                puls    PC,X,B,A        ; Pull of PC, effective RTS
L0691           lda     ,X+
                cmpa    #$20
                beq     L0691

                leax    -1,X
                rts
L069A           lda     ,Y+
                cmpa    #$20
                beq     L069A

                leay    -1,Y
                rts
L06A3           pshs    B,A
                ldd     <u001C
                sty     <u001C
                bsr     L06B2

                pshs    CC
                std     <u001C
                puls    PC,B,A,CC       ; Pull of PC, effective RTS
L06B2           pshs    A
L06B4           cmpx    <u001C
                beq     L06C0

                lda     ,X+
                cmpa    #$0D
                bne     L06B4

                andcc   #$FB
L06C0           puls    PC,A            ; Pull of PC, effective RTS
L06C2           cmpx    <u001C
                beq     L06CE

                bsr     L06B2

                cmpx    <u0018
                beq     L06CE

                leax    -1,X
L06CE           andcc   #$FB
                rts
L06D1           bsr     L06D7

                beq     L06E9

                leax    -1,X
L06D7           pshs    A
L06D9           cmpx    <u0018
                beq     L06E7

                lda     ,-X
                cmpa    #$0D
                bne     L06D9

                leax    1,X
                andcc   #$FB
L06E7           puls    A
L06E9           rts
L06EA           pshs    Y,X,B,A
                ldd     #$000B
                ldy     <u001C
                lbsr    L0BC0

                leax    D,Y
                sty     <u0016
                stx     <u001C
                stx     <u0018
                stx     <u001A
                std     ,Y
                std     2,Y
                lda     ,S
                sta     4,Y
                ldd     4,S
                std     5,Y
                ldd     <u0000
                std     <u0004
                ldd     <u0002
                std     <u0006
                puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L0716           pshs    Y,X,B,A
                stx     <u001A
                ldd     ,X
                lbsr    L0BEC

                nega
                negb
                sbca    #$00
                ldx     <u001C
                leax    D,X
                stx     <u0016
                leay    11,X
                sty     <u0018
                ldd     2,X
                leay    D,X
                sty     <u001A
                ldd     7,X
                std     <u0004
                ldd     9,X
                std     <u0006
                puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L073F           pshs    Y,X,B,A
                ldx     <u0016
                ldd     <u001C
                subd    <u0016
                std     ,X
                ldd     <u001A
                subd    <u0016
                std     2,X
                ldd     <u0004
                std     7,X
                ldd     <u0006
                std     9,X
                lda     4,X
                cmpa    #$42
                bne     L0763

                ldd     5,X
                std     <u0024
                bra     L076E

L0763           ldy     <u0014
                sty     <u001A
                ldd     ,X
                lbsr    L0C38

L076E           puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L0770           pshs    B,A
                ldx     <u0014
L0774           lda     4,X
                cmpa    #$42
                bne     L0781

                ldd     ,S
                cmpd    5,X
                beq     L0788

L0781           lbsr    L089B

                blo     L0774

                andcc   #$FB
L0788           puls    PC,B,A          ; Pull of PC, effective RTS
                ldx     <u0012
                ldd     u0005,U
                addd    #$041F
                leax    D,X
                cmpx    <u001C
                bls     L07A1

                os9     F$Mem
                lblo    L12AA

                sty     <u001E
L07A1           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0922

                lbsr    L00FF

                ldd     <u001C
                subd    <u0014
                lbsr    L0626

                lda     #$20
                lbsr    L0107

                lbsr    L0107

                ldd     <u001E
                subd    <u0014
                lbsr    L0626

                lbsr    L00FF

                lbra    L0929

                ldx     <u0016
                lda     4,X
                cmpa    #$42
                lbne    L127E

                ldd     <u001C
                subd    <u0016
                std     ,X
                ldd     u0005,U
                cmpd    5,X
                beq     L07F9

                bsr     L0770

                beq     L07F3

                ldd     #$000B
                lbsr    L0BC0

                lbsr    L073F

                lda     #$42
                ldy     u0005,U
                lbsr    L06EA

                pulu    PC,S            ; Pull of PC, effective RTS
L07F3           lbsr    L073F

                lbsr    L0716

L07F9           pulu    PC,S            ; Pull of PC, effective RTS
                tst     <u0041
                lbne    L127E

                ldx     <u0016
                lda     4,X
                cmpa    #$42
                lbne    L127E

                ldx     5,X
                ldy     <u0024
                pshs    Y,X
                ldx     u0005,U
                lbsr    L0691

                cmpa    1,X
                bne     L0825

                lbsr    L073F

                lda     #$4D
                lbsr    L06EA

                bra     L0836

L0825           leax    1,X
                lbsr    L04A1

                lbeq    L1278

                lbsr    L073F

                tfr     D,X
                lbsr    L0716

L0836           puls    Y,X
                stx     <u0024
                sty     <u002D
                lda     #$4D
                sta     <u0026
                pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0922

                leax    L13C6,PCR
                lbsr    L00EF

                ldx     <u0014
L084F           ldb     4,X
                cmpb    #$42
                bne     L0873

                ldd     5,X
                cmpx    <u0016
                beq     L0863

                cmpd    <u0024
                beq     L0866

                lda     #$20
L0862           cmpx    #$862A
L0863 equ L0862+1
L0865           cmpx    #$8624
L0866 equ L0865+1
                lbsr    L0107

                ldd     5,X
                lbsr    L0626

                lbsr    L00FF

L0873           bsr     L089B

                blo     L084F

                leax    L13BE,PCR
                lbsr    L00EF

                ldx     <u0014
L0880           pshs    X
                lda     4,X
                cmpa    #$4D
                bne     L0893

                leax    11,X
                lbsr    L008B

                puls    X
                bsr     L089B

                blo     L0880

L0893           lbsr    L00FF

                lbsr    L0929

                pulu    PC,S            ; Pull of PC, effective RTS
L089B           pshs    B,A
                ldd     ,X
                leax    D,X
                cmpx    <u001C
                puls    PC,B,A          ; Pull of PC, effective RTS
                tst     <u0041
                lbne    L127E

                ldx     u0005,U
                lda     ,X+
                pshs    A
                lbsr    L0691

                lbsr    L04A1

                lbeq    L1278

                tfr     D,Y
                ldd     ,Y
                ldx     <u001A
                pshs    X,B,A
                sty     <u001A
                lbsr    L0BEC

                ldd     <u0016
                subd    ,S
                std     <u0016
                ldd     <u0018
                subd    ,S
                std     <u0018
                ldd     <u001C
                subd    ,S
                std     <u001C
                puls    X
                puls    B,A
                pshs    X
                subd    ,S++
                std     <u001A
                pulu    PC,S            ; Pull of PC, effective RTS
                pshs    U
                ldx     u0005,U
                lbsr    L0130

                tfr     D,Y
                tfr     X,U
                leax    <L090C,PCR
                lda     #$01
                ldb     #$00
                os9     F$Fork
                lblo    L12AA

                os9     F$Wait
                tstb
                lbne    L12AA

                puls    U
                pulu    PC,S            ; Pull of PC, effective RTS
L090C           comb
                lsla
                FCB        $45    ; PC=890E INVALID opcode 45

                inca
                inca
                tst     <u00EC
                FCB        $45    ; PC=8913 INVALID opcode 45

                beq     L0918

                lda     #$01
L0918           sta     <u003E
                tst     <u0041
                bne     L0920

                sta     <u0040
L0920           pulu    PC,S            ; Pull of PC, effective RTS
L0922           lda     <u003E
                sta     <u003F
                inc     <u003E
                rts
L0929           lda     <u003F
                sta     <u003E
                rts
                ldx     ,U
                ldx     2,X
                ldy     <u0045
                cmpy    <u0049
                lbls    L128D

                stx     ,--Y
                ldx     #$0000
                stx     ,--Y
                sty     <u0045
                pulu    PC,S            ; Pull of PC, effective RTS
                ldx     <u0045
                ldd     ,X
                addd    #$0001
                std     ,X
                cmpd    u0005,U
                blo     L095A

                bsr     L0964

                pulu    PC,S            ; Pull of PC, effective RTS
L095A           ldy     2,X
                ldx     ,U
                sty     2,X
                pulu    PC,S            ; Pull of PC, effective RTS
L0964           pshs    X
                ldx     <u0045
                leax    4,X
                cmpx    <u0043
                lbhi    L128A

                stx     <u0045
                puls    PC,X            ; Pull of PC, effective RTS
                bsr     L0922

                ldx     <u001A
                lbsr    L0660

                beq     L098A

L097D           lbsr    L008B

                lbsr    L06B2

                beq     L098A

                lbsr    L0668

                bne     L097D

L098A           bsr     L0929

                pulu    PC,S            ; Pull of PC, effective RTS
                bsr     L0922

                lbsr    L0660

                beq     L09AF

                ldx     <u001A
                lbsr    L06D7

                bra     L09A1

L099C           lbsr    L06D1

                beq     L09A6

L09A1           lbsr    L0668

                bne     L099C

L09A6           pshs    X
                ldd     <u001A
                subd    ,S++
                lbsr    L0094

L09AF           lbsr    L0929

                pulu    PC,S            ; Pull of PC, effective RTS
                inc     <u002F
                ldd     u0005,U
                std     <u0030
                bne     L09BE

                clr     <u002F
L09BE           pulu    PC,S            ; Pull of PC, effective RTS
L09C0           lbsr    L06B2

                beq     L09D4

L09C5           pshs    B,A
                ldd     <u0030
                lbsr    L0B01

                puls    B,A
                bne     L09C0

                cmpx    <u001A
                blo     L09C0

L09D4           rts
L09D5           pshs    Y,X
                ldx     <u0010
                stx     2,S
                puls    PC,X            ; Pull of PC, effective RTS
L09DD           pshs    Y,B,A
                ldx     <u001A
                lda     ,Y+
                tst     <u002F
                beq     L09E9

                bsr     L09C5

L09E9           pshs    Y,X
L09EB           cmpa    ,Y
                beq     L0A0B

                ldb     ,Y+
                cmpx    <u001C
                bhs     L0A05

                cmpb    ,X+
                beq     L09EB

                puls    Y,X
                leax    1,X
                tst     <u002F
                beq     L09E9

                bsr     L09C0

                bra     L09E9

L0A05           orcc    #$01
                bra     L0A0B

                andcc   #$FE
L0A0B           puls    Y,X
                puls    PC,Y,B,A        ; Pull of PC, effective RTS
L0A0F           lbsr    L1126

                tst     <u0041
                bne     L0A29

                tst     <u003D
                beq     L0A29

                inc     <u0039
                lbsr    L0922

                leax    L1318,PCR
                lbsr    L00EF

                lbsr    L0929

L0A29           pulu    PC,S            ; Pull of PC, effective RTS
L0A2B           pshs    Y,X,B,A
                lbsr    L0660

                andcc   #$FE
                beq     L0A4E

                ldx     u0005,U
                lbsr    L0122

L0A39           ldy     u0005,U
                bsr     L09D5

                blo     L0A4E

                lbsr    L0082

                leax    D,X
                stx     <u001A
                lbsr    L0668

                bne     L0A39

                andcc   #$FE
L0A4E           puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
L0A50           pshs    Y,X,B,A
                lbsr    L0660

                andcc   #$FE
                beq     L0A94

                ldx     u0005,U
                lbsr    L0122

                pshs    B,A
                ldx     u0007,U
                lbsr    L0122

                pshs    B,A
L0A67           ldd     2,S
                subd    ,S
                lbsr    L0BC0

                ldy     u0007,U
                lbsr    L09D5

                blo     L0A92

                stx     <u001A
                ldd     ,S
                lbsr    L100F

                ldx     u0005,U
                leax    1,X
                ldd     2,S
                lbsr    L0BE3

                ldx     <u001A
                lbsr    L0082

                lbsr    L0668

                bne     L0A67

                andcc   #$FE
L0A92           leas    4,S
L0A94           puls    PC,Y,X,B,A      ; Pull of PC, effective RTS
                lbsr    L0A2B

                lblo    L0A0F

                rts
                lbsr    L0A50

                lblo    L0A0F

                rts
                ldx     <u001A
                lbsr    L0A50

                bra     L0AB2

                ldx     <u001A
                lbsr    L0A2B

L0AB2           pshs    CC
                cmpx    <u001A
                beq     L0ABF

                ldx     <u001A
                lbsr    L06D7

                stx     <u001A
L0ABF           puls    CC
                lblo    L0A0F

                rts
                lbsr    L0660

                beq     L0AFF

                ldx     u0005,U
                lbsr    L0122

                leax    1,X
L0AD2           pshs    X,B,A
                ldx     <u001A
                cmpx    <u001C
                blo     L0ADF

                lbsr    L1126

                bra     L0AFF

L0ADF           lbsr    L06C2

                ldd     ,S
                lbsr    L0BC0

                stx     <u001A
                ldx     2,S
                lbsr    L0BE3

                ldx     <u001A
                lbsr    L0082

                lbsr    L06B2

                stx     <u001A
                puls    X,B,A
                lbsr    L0668

                bne     L0AD2

L0AFF           pulu    PC,S            ; Pull of PC, effective RTS
L0B01           pshs    Y
                cmpd    #$0000
                beq     L0B26

                tfr     D,Y
                lbsr    L06D7

L0B0E           lda     ,X
                cmpa    #$0D
                beq     L0B20

                cmpx    <u001C
                bhs     L0B20

                leax    1,X
                leay    -1,Y
                bne     L0B0E

                leax    -1,X
L0B20           tfr     Y,D
                cmpd    #$0000
L0B26           puls    PC,Y            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0B5C

                ldx     <u001A
                ldd     u0005,U
                bsr     L0B01

                stx     <u001A
                std     u0005,U
                beq     L0B5C

                tfr     D,Y
                lbsr    L0668

                beq     L0B5C

                leay    -1,Y
                lda     #$20
                ldx     <u001C
                pshs    X
L0B48           cmpx    <u001E
                lbhs    L126F

                sta     ,X+
                lbsr    L0668

                bne     L0B48

                tfr     Y,D
                puls    X
                lbsr    L0BE3

L0B5C           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0B94

                ldx     <u001A
                lbsr    L06D7

                pshs    X
                ldx     u0005,U
                lbsr    L0122

                leax    1,X
                ldy     <u001C
L0B74           bsr     L0B96

                lbeq    L126F

                lbsr    L0668

                bne     L0B74

                ldx     <u001C
                tfr     Y,D
                subd    <u001C
                bsr     L0BE3

                lda     #$0D
                bsr     L0BB5

                ldx     ,S
                ldd     <u001A
                subd    ,S++
                lbsr    L0094

L0B94           pulu    PC,S            ; Pull of PC, effective RTS
L0B96           pshs    U,X,B,A
                tfr     D,U
L0B9A           cmpy    <u001E
                bhs     L0BB1

                cmpu    #$0000
                beq     L0BAD

                lda     ,X+
                sta     ,Y+
                leau    -1,U
                bra     L0B9A

L0BAD           andcc   #$FB
                puls    PC,U,X,B,A      ; Pull of PC, effective RTS
L0BB1           orcc    #$04
                puls    PC,U,X,B,A      ; Pull of PC, effective RTS
L0BB5           pshs    X,B,A
                tfr     S,X
                ldd     #$0001
                bsr     L0BE3

                puls    PC,X,B,A        ; Pull of PC, effective RTS
L0BC0           pshs    X
                ldx     <u001C
                leax    D,X
                cmpx    <u001E
                lbhs    L126F

                puls    PC,X            ; Pull of PC, effective RTS
L0BCE           pshs    Y
                ldy     <u001C
                bsr     L0B96

                lbeq    L126F

                sty     <u001C
                puls    PC,Y            ; Pull of PC, effective RTS
L0BDE           lda     #$0D
                lbsr    L0132

L0BE3           bsr     L0BCE

                bsr     L0C38

                addd    <u001A
                std     <u001A
                rts
L0BEC           pshs    U,Y,X,B,A
                cmpd    #$0000
                beq     L0C36

                std     <u00CF
                ldd     <u001A
                subd    <u001C
                tfr     D,Y
                addd    ,S
                std     <u00D1
                ldd     <u001C
                subd    <u00CF
                std     <u00D3
                ldx     <u001C
                lda     ,-X
                stx     <u00D5
                sta     <u00CD
                bra     L0C22

L0C10           cmpx    <u00D5
                bne     L0C1E

                lda     <u00CD
                sta     ,U
                lda     ,-X
                stx     <u00D5
                sta     <u00CD
L0C1E           leay    1,Y
                beq     L0C36

L0C22           ldd     <u00D1
L0C24           tfr     X,U
                leax    D,X
                lda     ,X
                sta     ,U
                cmpx    <u00D3
                bhs     L0C10

                ldd     <u00CF
                leay    1,Y
                bne     L0C24

L0C36           puls    PC,U,Y,X,B,A    ; Pull of PC, effective RTS
L0C38           pshs    B,A
                ldd     <u001C
                subd    <u001A
                subd    ,S
                bsr     L0BEC

                puls    PC,B,A          ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0CAF

                ldd     <u0024
                lbsr    L0770

                pshs    X
                ldd     ,X
                leay    D,X
                leax    11,X
                pshs    X
L0C58           pshs    Y
                cmpx    ,S++
                bne     L0C63

                lbsr    L1126

                bra     L0C6D

L0C63           lbsr    L06A3

                beq     L0C6D

                lbsr    L0668

                bne     L0C58

L0C6D           tfr     X,D
                subd    ,S
                puls    Y,X
                pshs    B,A
                lbsr    L0094

                ldd     2,Y
                subd    #$000B
                subd    ,S
                bhs     L0C84

                ldd     #$0000
L0C84           addd    #$000B
                std     2,Y
                ldd     ,Y
                subd    ,S
                std     ,Y
                ldd     <u0016
                subd    ,S
                std     <u0016
                ldd     <u0018
                subd    ,S
                std     <u0018
                puls    B,A
                ldy     <u001A
                stx     <u001A
                ldx     <u001C
                sty     <u001C
                lbsr    L0BEC

                stx     <u001C
                sty     <u001A
L0CAF           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0D0D

                ldx     <u001C
                pshs    X
                ldd     <u0024
                lbsr    L0770

                pshs    X
                ldx     <u001A
                pshs    X
L0CC5           cmpx    <u001C
                blo     L0CCE

                lbsr    L1126

                bra     L0CD8

L0CCE           lbsr    L06B2

                beq     L0CD8

                lbsr    L0668

                bne     L0CC5

L0CD8           tfr     X,D
                subd    ,S
                puls    Y,X
                lbsr    L0094

                leax    D,X
                pshs    X,B,A
                stx     <u001C
                ldd     ,Y
                addd    ,S
                std     ,Y
                ldd     2,Y
                leax    D,Y
                stx     <u001A
                addd    ,S
                std     2,Y
                ldd     <u0016
                addd    ,S
                std     <u0016
                ldd     <u0018
                addd    ,S
                std     <u0018
                puls    Y,X,B,A
                lbsr    L0C38

                stx     <u001A
                sty     <u001C
L0D0D           pulu    PC,S            ; Pull of PC, effective RTS
                tst     <u0005
                beq     L0D4D

                lbsr    L0660

                beq     L0D7A

                ldx     <u001C
L0D1A           leay    128,X
                cmpy    <u001E
                bls     L0D28

                bsr     L0D68

                lbra    L126F

L0D28           lda     <u0004
                ldy     #$0080
                os9     I$ReadLn
                bhs     L0D5F

                pshs    B
                bsr     L0D68

                puls    B
                cmpb    #$D3
                lbne    L12AA

                ldd     <u0004
                cmpd    <u0000
                beq     L0D4D

                os9     I$Close
                lblo    L12AA

L0D4D           clr     <u0005
                lbsr    L1126

                tst     <u0041
                bne     L0D5D

                leax    L138A,PCR
                lbsr    L00EF

L0D5D           bra     L0D7A

L0D5F           tfr     Y,D
                leax    D,X
                lbsr    L0668

                bne     L0D1A

L0D68           tfr     X,D
                subd    <u001C
                ldx     <u001A
                pshs    X,B,A
                ldx     <u001C
                lbsr    L0BE3

                puls    X,B,A
                lbsr    L0094

L0D7A           rts
                tst     <u0007
                lbeq    L1275

                lbsr    L0660

                beq     L0DB5

                ldy     <u001A
L0D89           tfr     Y,X
                lbsr    L0130

                bne     L0D9D

                bsr     L0DA9

                lbsr    L1126

                leax    L137C,PCR
                lbsr    L00EF

                rts
L0D9D           leay    D,X
                lda     <u0006
                lbsr    L00C8

                lbsr    L0668

                bne     L0D89

L0DA9           ldx     <u001A
                tfr     Y,D
                subd    <u001A
                lbsr    L0094

                lbsr    L100F

L0DB5           rts
                ldd     <u0004
                cmpd    <u0000
                beq     L0DC9

                tstb
                beq     L0DC9

                os9     I$Close
                lblo    L12AA

                clr     <u0005
L0DC9           ldx     u0005,U
                ldb     ,X+
                cmpb    ,X
                bne     L0DD7

                ldd     <u0000
                std     <u0004
                pulu    PC,S            ; Pull of PC, effective RTS
L0DD7           lbsr    L0EC6

                lbne    L12A8

                lda     #$01
                os9     I$Open
                lblo    L12AA

                ldb     #$01
                std     <u0004
                pulu    PC,S            ; Pull of PC, effective RTS
                ldd     <u0006
                cmpd    <u0002
                beq     L0E00

                tstb
                beq     L0E00

                os9     I$Close
                lblo    L12AA

                clr     <u0007
L0E00           ldx     u0005,U
                ldb     ,X+
                cmpb    ,X
                bne     L0E0E

                ldd     <u0002
                std     <u0006
                pulu    PC,S            ; Pull of PC, effective RTS
L0E0E           lbsr    L0EC6

                lbne    L12A8

                ldd     #$021B
                os9     I$Create
                lblo    L12AA

                ldb     #$01
                std     <u0006
                pulu    PC,S            ; Pull of PC, effective RTS
                ldx     u0005,U
                ldb     ,X+
                lbsr    L0EC6

                lbne    L12A8

                lda     #$01
                os9     I$Open
                lblo    L12AA

                sta     <u0008
L0E3B           ldx     <u001C
                ldy     #$000B
                bsr     L0EB7

                lda     <u0008
                os9     I$Read
                blo     L0E6E

                lda     4,X
                cmpa    #$4D
                beq     L0E58

                ldd     5,X
                beq     L0E58

                ldb     #$D3
                bra     L0E6E

L0E58           clr     8,X
                clr     10,X
                ldd     ,X
                subd    #$000B
                tfr     D,Y
                leax    11,X
                bsr     L0EB7

                lda     <u0008
                os9     I$Read
                bhs     L0E7F

L0E6E           pshs    B
                lda     <u0008
                os9     I$Close
                puls    B
                cmpb    #$D3
                lbne    L12AA

                pulu    PC,S            ; Pull of PC, effective RTS
L0E7F           lbsr    L0691

                pshs    X
                lbsr    L04A1

                puls    X
                bne     L0E3B

                lbsr    L008B

                ldx     <u001C
                ldd     ,X
                ldy     <u001A
                leax    D,X
                stx     <u001C
                pshs    Y,B,A
                ldx     <u0014
                stx     <u001A
                lbsr    L0C38

                ldd     <u0016
                addd    ,S
                std     <u0016
                ldd     <u0018
                addd    ,S
                std     <u0018
                puls    Y,B,A
                leay    D,Y
                sty     <u001A
                bra     L0E3B

L0EB7           pshs    Y,B,A
                tfr     Y,D
                leay    D,X
                cmpy    <u001E
                lbhs    L126F

                puls    PC,Y,B,A        ; Pull of PC, effective RTS
L0EC6           lbsr    L0691

                pshs    Y,X
                leay    ,X
L0ECD           cmpb    ,Y+
                bne     L0ECD

                pshs    Y
                lbsr    L015A

                blo     L0EE1

                cmpy    ,S++
                bhs     L0EE1

                orcc    #$04
                puls    PC,Y,X          ; Pull of PC, effective RTS
L0EE1           andcc   #$FB
                puls    PC,Y,X          ; Pull of PC, effective RTS
                ldx     #$FFFF
                pshs    X
                ldx     u0007,U
                leax    1,X
                lbsr    L0691

L0EF1           lbsr    L04A1

                lbeq    L1278

                pshs    B,A
                lbsr    L0691

                ldy     u0007,U
                cmpa    ,Y
                bne     L0EF1

                ldx     u0005,U
                ldb     ,X+
                bsr     L0EC6

                lbne    L12A8

                ldd     #$021B
                os9     I$Create
                lblo    L12AA

                sta     <u0008
L0F1A           puls    X
                cmpx    #$FFFF
                beq     L0F3C

                ldd     #$0000
                std     5,X
                ldy     ,X
                lda     <u0008
                os9     I$Write
                bhs     L0F1A

                pshs    B
                lda     <u0008
                os9     I$Close
                puls    B
                lbra    L12AA

L0F3C           lda     <u0008
                os9     I$Close
                pulu    PC,S            ; Pull of PC, effective RTS
L0F43           tst     <u0001
                beq     L0F8E

                ldx     <u001C
                leay    D,X
                leay    128,Y
                cmpy    <u001E
                blo     L0F5D

                cmpd    #$0080
                blo     L0F8E

                subd    #$0080
L0F5D           tfr     D,Y
                lda     <u0000
                os9     I$Read
                blo     L0F77

                tfr     Y,D
                leax    D,X
                stx     <u001C
                ldy     #$0080
                lda     <u0000
                os9     I$ReadLn
                bhs     L0F88

L0F77           cmpb    #$D3
                lbne    L12AA

                leax    L138A,PCR
                lbsr    L00EF

                clr     <u0005
                bra     L0F8E

L0F88           tfr     Y,D
                leax    D,X
                stx     <u001C
L0F8E           rts
                tst     <u0003
                beq     L0FA8

                ldx     <u0018
                ldd     <u001A
                subd    <u0018
                tfr     D,Y
                lda     <u0002
                os9     I$Write
                stx     <u001A
                tfr     Y,D
                bsr     L100F

                bsr     L0F43

L0FA8           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0FD4

                ldx     <u001A
                lbsr    L06D7

                stx     <u001A
                pshs    X
L0FB8           cmpx    <u001C
                bne     L0FC1

                lbsr    L1126

                bra     L0FCE

L0FC1           lbsr    L008B

                lbsr    L06B2

                beq     L0FCE

                lbsr    L0668

                bne     L0FB8

L0FCE           tfr     X,D
                subd    ,S++
                bsr     L100F

L0FD4           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L0FFA

                ldx     <u001A
                pshs    X
L0FDF           cmpx    <u001C
                bne     L0FE8

                lbsr    L1126

                bra     L0FEF

L0FE8           leax    1,X
                lbsr    L0668

                bne     L0FDF

L0FEF           tfr     X,D
                subd    ,S
                puls    X
                lbsr    L0094

                bsr     L100F

L0FFA           pulu    PC,S            ; Pull of PC, effective RTS
                ldx     <u001A
                lbsr    L0130

                beq     L100D

                subd    #$0001
                beq     L100D

                bsr     L100F

                lbsr    L0082

L100D           pulu    PC,S            ; Pull of PC, effective RTS
L100F           pshs    B,A
                lbsr    L0BEC

                ldd     <u001C
                subd    ,S
                std     <u001C
                puls    PC,B,A          ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L1035

                ldx     <u001A
L1023           cmpx    <u0018
                bne     L102C

                lbsr    L1126

                bra     L1033

L102C           leax    -1,X
                lbsr    L0668

                bne     L1023

L1033           stx     <u001A
L1035           pulu    PC,S            ; Pull of PC, effective RTS
                lbsr    L0660

                beq     L1035

                ldx     <u001A
L103E           cmpx    <u001C
                bne     L1047

                lbsr    L1126

                bra     L1033

L1047           leax    1,X
                lbsr    L0668

                bne     L103E

                bra     L1033

                ldx     <u001A
                lbsr    L0660

                bne     L105C

                lbsr    L06C2

                bra     L106F

L105C           cmpx    <u001C
                bne     L1065

                lbsr    L1126

                bra     L106F

L1065           lbsr    L06B2

                beq     L106F

                lbsr    L0668

                bne     L105C

L106F           stx     <u001A
                lbsr    L008B

                pulu    PC,S            ; Pull of PC, effective RTS
                ldx     <u001A
                lbsr    L0660

                bne     L1082

                lbsr    L06D7

                bra     L106F

L1082           cmpx    <u0018
                bne     L108B

                lbsr    L1126

                bra     L106F

L108B           lbsr    L06D1

                beq     L106F

                lbsr    L0668

                bne     L1082

                bra     L106F

                ldx     <u0018
                stx     <u001A
                pulu    PC,S            ; Pull of PC, effective RTS
                ldx     <u001C
                stx     <u001A
                pulu    PC,S            ; Pull of PC, effective RTS
                lda     #$01
                sta     <u003C
                clr     <u003A
                pulu    PC,S            ; Pull of PC, effective RTS
L10AB           lda     #$01
                sta     <u003D
                clr     <u003A
                pulu    PC,S            ; Pull of PC, effective RTS
L10B3           clra
                sta     <u003D
                sta     <u003A
                pulu    PC,S            ; Pull of PC, effective RTS
                ldx     <u001A
                cmpx    <u001C
                beq     L10AB

                lda     ,X
                cmpa    #$0D
                beq     L10AB

                bra     L10B3

                ldx     <u001A
                cmpx    <u001C
                beq     L10B3

                lda     ,X
                cmpa    #$0D
                beq     L10B3

                bra     L10AB

                ldx     <u001A
                cmpx    <u001C
                bne     L10B3

                bra     L10AB

                ldx     <u001A
                cmpx    <u001C
                beq     L10B3

                bra     L10AB

                bsr     L10F2

                bne     L10AB

                bra     L10B3

                bsr     L10F2

                beq     L10AB

                bra     L10B3

L10F2           ldx     u0005,U
                ldy     <u001A
                ldb     ,X+
L10F9           cmpb    ,X
                beq     L110A

                cmpy    <u001C
                beq     L1108

                lda     ,X+
                cmpa    ,Y+
                beq     L10F9

L1108           andcc   #$FB
L110A           rts
                tst     <u0005
                bne     L10B3

                bra     L10AB

                lda     <u0005
                beq     L10B3

                bra     L10AB

                ldd     u0005,U
                beq     L10B3

                bra     L10AB

                lda     #$00
                lbsr    L0685

                beq     L10B3

                bra     L10AB

L1126           pshs    A
                lbsr    L0685

                beq     L1133

                lda     #$01
                sta     <u003D
                clr     <u003A
L1133           puls    PC,A            ; Pull of PC, effective RTS
                lda     #$00
                sta     <u003D
                sta     <u003A
                inca
                sta     <u003B
                pulu    PC,S            ; Pull of PC, effective RTS
                lda     #$00
                sta     <u003A
                inca
                sta     <u003D
                sta     <u003B
                pulu    PC,S            ; Pull of PC, effective RTS
                pulu    PC,S            ; Pull of PC, effective RTS
L114D           tst     <u0041
                lbne    L127E

                ldx     <u0016
                lda     4,X
                cmpa    #$42
                beq     L11C5

                ldx     <u001C
                cmpx    <u0018
                lbeq    L1281

                lda     #$0D
                cmpa    -1,X
                beq     L1175

                leax    1,X
                cmpx    <u001E
                lbhs    L126F

                sta     -1,X
                stx     <u001C
L1175           ldx     <u0018
                lbsr    L0691

                lbsr    L04DA

                lbne    L1281

                pshs    X
L1183           lda     ,X+
                lbsr    L04DA

                beq     L1183

                cmpa    #$20
                beq     L119C

                cmpa    #$0D
                beq     L119C

                cmpa    #$24
                beq     L119C

                cmpa    #$23
                lbne    L1281

L119C           ldx     ,S
                lbsr    L04A1

                lbne    L1284

                ldb     #$2E
                puls    X
                lbsr    L0415

                lbeq    L1284

                lbsr    L073F

                ldd     <u0024
                lbsr    L0770

                lbsr    L0716

                ldd     <u002D
                std     <u0024
                lda     #$45
                sta     <u0026
                pulu    PC,S            ; Pull of PC, effective RTS
L11C5           ldd     #$0001
                lbsr    L0770

                cmpx    <u0016
                beq     L11D2

                lbra    L126C

L11D2           ldy     <u0012
                leay    137,Y
L11D9           ldx     ,Y++
                beq     L11E4

                jsr     ,X

                os9     F$UnLink
                bra     L11D9

L11E4           ldx     <u0018
                ldd     <u001C
                subd    <u0018
                tfr     D,Y
L11EC           ldd     <u0002
                cmpb    #$00
                beq     L1261

                os9     I$Write
                blo     L1263

                ldx     <u0014
                ldd     <u001E
                subd    <u0014
                tfr     D,Y
                ldd     <u0000
                cmpb    #$00
                beq     L1261

                os9     I$Read
                bhs     L11EC

                cmpb    #$D3
                bne     L1263

                tst     <u0036
                beq     L1261

                ldd     <u001E
                subd    <u0014
                os9     F$Mem
                blo     L1263

                lda     <u0000
                os9     I$Close
                blo     L1263

                lda     <u0002
                os9     I$Close
                blo     L1263

                ldx     <u0032
                os9     I$Delete
                blo     L1261

                ldy     <u0032
L1233           ldx     <u0032
L1235           lda     ,Y+
                sta     ,X+
                cmpa    #$2F
                beq     L1233

                cmpa    #$0D
                bne     L1235

                ldy     <u0012
                leax    254,Y
                tfr     X,U
                lbsr    L0130

                leax    247,Y
                tfr     D,Y
                ldd     #$0100
                os9     F$Fork
                blo     L1263

                os9     F$Wait
                tstb
                bne     L1263

L1261           ldb     #$00
L1263           os9     F$Exit
L1266           ldb     #$00
L1268           cmpx    #$C60B
L1269 equ L1268+1
L126B           cmpx    #$C613
L126C equ L126B+1
L126E           cmpx    #$C622
L126F equ L126E+1
L1271           cmpx    #$C633
L1272 equ L1271+1
L1274           cmpx    #$C641
L1275 equ L1274+1
L1277           cmpx    #$C64F
L1278 equ L1277+1
L127A           cmpx    #$C659
L127B equ L127A+1
L127D           cmpx    #$C65F
L127E equ L127D+1
L1280           cmpx    #$C66D
L1281 equ L1280+1
L1283           cmpx    #$C67C
L1284 equ L1283+1
L1286           cmpx    #$C687
L1287 equ L1286+1
L1289           cmpx    #$C694
L128A equ L1289+1
L128C           cmpx    #$C694
L128D equ L128C+1
L128F           cmpx    #$C6A5
L1290 equ L128F+1
                lda     #$FF
                sta     <u0023
                inc     <u003E
                leax    L12C9,PCR
                clra
                leax    D,X
                lbsr    L00EF

                lbsr    L00FF

                lbra    L0292

L12A8           ldb     #$D7
L12AA           inc     <u003E
                pshs    B
                leax    <L12C5,PCR
                ldy     #$0004
                lda     <u000B
                os9     I$Write
                puls    B
                os9     F$PErr
                lbsr    L00FF

                lbra    L0292

L12C5           FCC     "OS9 "

L12C9           FCC     "BAD NUMBER"

                FCB     $0D

                FCC     "WHAT ??"

                FCB     $0D

                FCC     "* NOT BUF #1 *"

                FCB     $0D

                FCC     "*WORKSPACE FULL*"

                FCB     $0D

                FCC     "MISSING DELIM"

                FCB     $0D

                FCC     "*FILE CLOSED*"

                FCB     $0D

L1318           FCC     "NOT FOUND"

                FCB     $0D

                FCC     "BREAK"

                FCB     $0D

                FCC     "MACRO IS OPEN"

                FCB     $0D

                FCC     "BAD MACRO NAME"

                FCB     $0D

                FCC     "DUPL MACRO"

                FCB     $0D

                FCC     "BAD VAR LIST"

                FCB     $0D

                FCC     "BRACKET MISMATCH"

                FCB     $0D

                FCC     "UNDEFINED VAR"

                FCB     $0D

L137C           FCC     "*END OF TEXT*"

                FCB     $0D

L138A           FCC     "*END OF FILE*"

                FCB     $0D

L1398           FCC     "*FAIL*"

                FCB     $0D

L139F           FCC     "RENAME "

                FCB     $0D

L13A7           FCC     "SCRATCH "

                FCB     $0D

L13B0           FCC     "EDTP2"

                FCB     $0D

L13B6           FCC     "EDTLIB1"

                FCB     $0D

L13BE           FCC     "MACROS:"

                FCB     $0D

L13C6           FCC     "BUFFERS:"

                FCB     $0D

L13CF           FCC     "MAC$"

                FCB     $0D,$07,$FB

                FCC     "EOF"

                FCB     $0D,$11,$11

                FCC     "NEOF"

                FCB     $0D,$11,$0B

                FCC     "EOB"

                FCB     $0D,$10,$DE

                FCC     "NEOB"

                FCB     $0D,$10,$D6

                FCC     "EOL"

                FCB     $0D,$10,$C8

                FCC     "NEOL"

                FCB     $0D,$10,$BA

                FCC     "ZERO#"

                FCB     $0D,$11,$17

                FCC     "STAR#"

                FCB     $0D,$11,$1D

                FCC     "STR$"

                FCB     $0D,$10,$E6

                FCC     "NSTR$"

                FCB     $0D,$10,$EC

                FCC     "DIR"

                FCB     $0D,$08,$43,$53,$0D,$11,$35,$46
                FCB     $0D,$11

                FCC     "@SEARCH#$"

                FCB     $0D,$0A,$96

                FCC     "CHANGE#$$"

                FCB     $0D,$0A,$9E

                FCC     "LOAD$"

                FCB     $0D,$0E

                FCC     "%SAVE$$"

                FCB     $0D,$0E,$E5

                FCC     "SIZE"

                FCB     $0D,$07,$A3

                FCC     "DEL$"

                FCB     $0D,$08,$A5

                FCC     "READ$"

                FCB     $0D,$0D,$B6

                FCC     "WRITE$"

                FCB     $0D,$0D,$ED

                FCC     "SHELL L"

                FCB     $0D,$08,$E7

                FCC     "NEW"

                FCB     $0D,$0F,$8F,$00

L1482           FCB     $41,$23,$0D,$09,$B4,$4C,$23,$0D
                FCB     $09,$74,$58,$23,$0D,$09,$8E,$2B
                FCB     $23,$0D,$10,$50,$2D,$23,$0D,$10
                FCB     $76,$44,$23,$0D,$0F,$AA,$45,$23
                FCB     $24,$0D,$0A,$C6,$3C,$23,$0D,$10
                FCB     $1C,$3E,$23,$0D,$10,$37,$49,$23
                FCB     $24,$0D,$0B,$5E,$4B,$23,$0D,$0F
                FCB     $D6,$53,$23,$24,$0D,$0A,$AD,$43
                FCB     $23,$24,$24,$0D,$0A,$A6,$55,$0D
                FCB     $0F,$FC,$54,$23,$0D,$0B,$28,$42
                FCB     $23,$0D,$07,$C5,$5E,$0D,$10,$97
                FCB     $2F,$0D,$10,$9D,$4D,$23,$0D,$07
                FCB     $8A,$56,$23,$0D,$09,$12,$47,$23
                FCB     $0D,$0C,$44,$50,$23,$0D,$0C,$B1
                FCB     $5B,$0D,$09,$2E,$5D,$23,$0D,$09
                FCB     $48,$3A,$0D,$10,$A3,$52,$23,$0D
                FCB     $0D,$0F,$57,$23,$0D,$0D,$7B,$21
                FCB     $4C,$0D,$11,$4B,$51,$0D,$11,$4D
                FCB     $00

BeebDisEndAddr
;  "edit_dis.bin",BeebDisStartAddr,BeebDisEndAddr

                emod
eom             equ     *
                end


; 8000 87 CD 15 16 00 0D 11 81 2B 01 61 0C 1F 45 64 69 --------+-a--Edi UUUUUUUUUUUUUDDD
; 8010 F4 03 28 43 29 31 39 38 31 4D 69 63 72 6F 77 61 --(C)1981Microwa DDDDDDDDDDDDDDDD
; 8020 72 65 01 16 02 6C 16 0C 0F 16 0B C0 16 0B B4 16 re---l---------- DDOOOOOOOOOOOOOO
; 8030 0F DD 16 07 3B 16 06 B2 16 06 DB 16 07 01 16 04 ----;----------- OOOOOOOOOOOOOOOO
; 8040 60 16 04 27 16 05 8F 16 05 DC 16 00 47 16 0B 46 `--'--------G--F OOOOOOOOOOOOOOOO
; 8050 16 0A AE 86 00 A7 C8 23 3B 9E 0E 6E 84 96 26 17 -------#;--n--&- OOOOOOOOOOOOOOOO
; 8060 00 A5 86 3A 17 00 A0 9E 12 30 89 01 77 96 0A 10 ---:-----0--w--- OOOOOOOOOOOOOOOO
; 8070 8E 00 80 10 3F 8B 24 09 C1 D3 10 27 10 CF 16 12 ----?-$----'---- OOOOOOOOOOOOOOOO
; 8080 29 39 34 16 17 06 50 8D 02 35 96 34 06 17 00 A0 )94---P--5-4---- OOOOOOOOOOOOOOOO
; 8090 8D 02 35 86 34 36 10 83 00 00 27 2A 31 8B 34 20 --5-46----'*1-4  OOOOOOOOOOOOOOOO
; 80A0 0D 37 27 08 86 20 8D 5F 8D 5D 8D 5B 0D 23 27 14 -7'-- -_-]-[-#'- OOOOOOOOOOOOOOOO
; 80B0 A6 80 8D 53 AC E4 27 06 81 0D 26 F0 20 E2 81 0D ---S--'---&- --- OOOOOOOOOOOOOOOO
; 80C0 27 02 8D 3B 35 20 35 B6 34 26 DC 02 5D 27 11 A1 '--;5 5-4&--]'-- OOOOOOOOOOOOOOOO
; 80D0 E4 26 0D DC 21 C3 00 01 DD 21 D6 20 C9 00 D7 20 -&--!----!- ---  OOOOOOOOOOOOOOOO
; 80E0 8D 4E 1F 02 A6 E4 10 3F 8A 10 25 11 BD 35 A6 34 -N-----?--%--5-4 OOOOOOOOOOOOOOOO
; 80F0 02 8D 0C 96 37 0F 37 8D 92 97 37 35 82 8D 00 34 ----7-7---75---4 OOOOOOOOOOOOOOOO
; 8100 02 86 0D 8D 02 35 82 34 32 96 0B 10 8E 00 01 1F -----5-42------- OOOOOOOOOOOOOOOO
; 8110 41 0D 41 2B 04 0D 3E 27 07 10 3F 8C 10 25 11 8A A-A+-->'--?--%-- OOOOOOOOOOOOOOOO
; 8120 35 B2 34 10 A6 80 81 0D 10 27 11 46 8D 04 35 90 5-4------'-F--5- OOOOOOOOOOOOOOOO
; 8130 86 0D 34 30 C6 0D 10 8E 00 00 9C 1C 27 0E 31 21 --40--------'-1! OOOOOOOOOOOOOOOO
; 8140 E1 84 27 08 A1 80 26 F2 31 3F 20 06 81 0D 10 26 --'---&-1? ----& OOOOOOOOOOOOOOOO
; 8150 11 20 1F 20 10 83 00 00 35 B0 34 56 10 3F 10 35 - - ----5-4V-?-5 OOOOOOOOOOOOOOOO
; 8160 D6 1F 30 DD 12 10 DF 1E 32 C9 04 1E C3 04 1F DD --0-----2------- OOOOOOOOOOOOOOOO
; 8170 14 DD 16 DD 1C DD 1A 34 76 30 8D FE A5 9F 0C 30 -------4v0-----0 OOOOOOOOOOOOOOOO
; 8180 8D FE DA 9F 0E 30 8D 08 54 9F 10 CC 00 00 DD 51 -----0--T------Q OOOOOOOOOOOOOOOO
; 8190 DD 6F DD 89 30 8D 12 EA 9F 4D 30 8D 12 31 9F 6B -o--0----M0--1-k OOOOOOOOOOOOOOOO
; 81A0 30 8D FE 5C 9F 4F 9F 6D 30 8D 12 04 86 01 10 3F 0--\-O-m0------? OOOOOOOOOOOOOOOO
; 81B0 00 25 02 AD A4 30 8D 11 FD 86 01 10 3F 00 25 02 -%---0------?-%- OOOOOOOOOOOOOOOO
; 81C0 AD A4 35 76 86 FF 97 41 4C 97 01 97 03 97 05 97 --5v---AL------- OOOOOOOOOOOOOOOO
; 81D0 07 97 0A 97 3D 4C 97 0B 97 3E 97 40 86 45 97 26 ----=L--->-@-E-& OOOOOOOOOOOOOOOO
; 81E0 0F 36 17 04 AC 81 0D 27 77 17 FF 6E 25 6B 86 01 -6-----'w--n%k-- OOOOOOOOOOOOOOOO
; 81F0 9F 32 10 3F 84 25 54 C6 01 DD 00 34 10 31 C9 00 -2-?-%T----4-1-- OOOOOOOOOOOOOOOO
; 8200 F7 30 8D 11 9A CC 00 07 17 09 8B 34 20 9E 32 AC -0---------4 -2- OOOOOOOOOOOOOOOO
; 8210 62 27 0D A6 80 A7 A0 81 2F 26 F4 10 AF E4 20 EF b'------/&---- - OOOOOOOOOOOOOOOO
; 8220 35 20 30 8D 11 81 CC 00 08 17 09 6A 9E 32 EC E4 5 0--------j-2-- OOOOOOOOOOOOOOOO
; 8230 93 32 10 9F 32 17 09 5E 86 0D A7 A4 35 10 17 04 -2--2--^----5--- OOOOOOOOOOOOOOOO
; 8240 50 81 0D 26 06 30 C9 00 FE 0C 36 CC 02 0B 10 3F P--&-0----6----? OOOOOOOOOOOOOOOO
; 8250 83 25 08 C6 02 DD 02 20 07 C6 D8 1A 01 10 3F 06 -%----- ------?- OOOOOOOOOOOOOOOO
; 8260 10 8E 00 00 10 9F 24 86 42 17 04 7E 17 04 D0 86 ------$-B--~---- OOOOOOOOOOOOOOOO
; 8270 42 10 8E 00 01 17 04 72 30 8D FD D7 DE 12 10 3F B------r0------? OOOOOOOOOOOOOOOO
; 8280 09 0D 01 27 0A DC 1E 93 1C 83 04 00 17 0C B4 17 ---'------------ OOOOOOOOOOOOOOOO
; 8290 FE 6B DE 12 32 C9 04 1E 30 C9 02 9E 9F 45 9F 47 -k--2---0----E-G OOOOOOOOOOOOOOOO
; 82A0 30 88 D8 9F 49 86 FF 97 41 97 23 96 40 97 3E 0F 0---I---A-#-@->- OOOOOOOOOOOOOOOO
; 82B0 3B 0F 3A 0F 3C 86 01 97 37 0D 3D 27 0E 0D 39 26 ;-:-<---7-='--9& OOOOOOOOOOOOOOOO
; 82C0 0A 30 8D 10 D3 17 FE 27 17 FE 34 0F 3D 0F 39 17 -0-----'--4-=-9- OOOOOOOOOOOOOOOO
; 82D0 FD 87 30 C9 01 77 17 FE 57 31 8B 10 9F 2B 10 8E --0--w--W1---+-- OOOOOOOOOOOOOOOO
; 82E0 00 00 10 9F 29 33 C9 02 76 DF 27 A6 84 81 20 26 ----)3--v-'--- & OOOOOOOOOOOOOOOO
; 82F0 07 30 01 17 08 E8 20 9A 81 0D 26 0C 9E 1A 17 03 -0---- ---&----- OOOOOOOOOOOOOOOO
; 8300 B1 9F 1A 17 FD 85 20 8A 8D 06 17 FD F2 16 FF 82 ------ --------- OOOOOOOOOOOOOOOO
; 8310 DC 43 34 06 DC 45 DD 43 34 06 96 3E 34 02 0F 2F -C4--E-C4-->4--/ OOOOOOOOOOOOOOOO
; 8320 0C 41 9C 2B 10 24 00 90 17 03 66 81 0D 26 06 30 -A-+-$----f--&-0 OOOOOOOOOOOOOOOO
; 8330 01 0F 2F 20 ED DC 27 34 06 DF 27 DC 29 34 06 DC --/ --'4--'-)4-- OOOOOOOOOOOOOOOO
; 8340 2B 34 06 34 40 17 03 49 97 38 17 00 86 34 10 34 +4-4@--I-8---4-4 OOOOOOOOOOOOOOOO
; 8350 40 30 8C 49 36 10 36 40 1F 01 96 38 0D 3B 26 12 @0-I6-6@---8-;&- OOOOOOOOOOOOOOOO
; 8360 0D 3C 26 0E 0D 3D 27 2F 0D 3A 26 06 81 3A 26 02 -<&--='/-:&--:&- OOOOOOOOOOOOOOOO
; 8370 D7 3D 81 5B 26 02 0C 3A 81 5D 26 21 0A 3A 2A 1D -=-[&--:-]&!-:*- OOOOOOOOOOOOOOOO
; 8380 0D 3C 26 11 17 05 DD 0D 3B 26 04 0F 3D 20 0E 0F -<&-----;&--= -- OOOOOOOOOOOOOOOO
; 8390 3B 20 0A 20 08 0F 3C 96 41 0F 39 AD A4 35 40 35 ; - --<-A-9--5@5 OOOOOOOOOOOOOOOO
; 83A0 10 35 40 35 06 DD 2B 35 06 DD 29 35 06 DD 27 0D -5@5--+5--)5--'- OOOOOOOOOOOOOOOO
; 83B0 23 10 27 0E C6 16 FF 6A 0A 41 35 02 97 3E 35 06 #-'----j-A5-->5- OOOOOOOOOOOOOOOO
; 83C0 DD 45 35 06 DD 43 0D 3D 27 08 86 01 97 3D 0F 3C -E5--C-='----=-< OOOOOOOOOOOOOOOO
; 83D0 0F 3B 39 17 02 BB 8D 36 27 26 30 01 17 00 C2 10 -;9----6'&0----- OOOOOOOOOOOOOOOO
; 83E0 27 0E 86 34 20 1F 02 EC A4 31 AB 10 9F 2B 10 AE '--4 ----1---+-- OOOOOOOOOOOOOOOO
; 83F0 E4 17 01 00 1F 20 35 20 10 9F 29 31 8D FF 11 39 ----- 5 --)1---9 OOOOOOOOOOOOOOOO
; 8400 34 06 17 00 EF CC 00 00 DD 29 DD 2B 35 A0 E6 80 4--------)-+5--- OOOOOOOOOOOOOOOO
; 8410 17 00 BD 1F 89 10 9E 12 31 A8 4D C1 2E 26 06 10 --------1-M-.&-- OOOOOOOOOOOOOOOO
; 8420 9E 12 31 A8 6B 10 9F 4B 10 AE A4 A6 A4 26 0D 10 --1-k--K-----&-- OOOOOOOOOOOOOOOO
; 8430 9E 4B 31 24 10 9F 4B 10 AE A4 27 21 C1 2E 27 06 -K1$--K---'!-.'- OOOOOOOOOOOOOOOO
; 8440 E1 A0 26 06 20 08 8D 23 27 04 8D 16 20 DD 34 20 --&- --#'--- -4  OOOOOOOOOOOOOOOO
; 8450 8D 10 EC 3E 10 9E 4B E3 22 1A 04 35 A0 30 1F 1C --->--K-"--5-0-- OOOOOOOOOOOOOOOO
; 8460 FB 39 A6 A0 81 0D 26 FA 31 22 39 34 36 A6 A4 8D -9----&-1"946--- OOOOOOOOOOOOOOOO
; 8470 69 26 2A 10 AF 64 A6 A0 8D 56 26 15 34 02 A6 80 i&*--d---V&-4--- OOOOOOOOOOOOOOOO
; 8480 8D 4E A1 E0 27 ED 10 AF 64 A6 A0 8D 4D 27 F7 20 -N--'---d---M'-  OOOOOOOOOOOOOOOO
; 8490 0C A6 84 8D 45 27 06 AF 62 1A 04 20 02 1C FB 35 ----E'--b-- ---5 OOOOOOOOOOOOOOOO
; 84A0 B6 34 40 DE 14 17 01 E9 A6 44 81 4D 26 12 31 4B -4@------D-M&-1K OOOOOOOOOOOOOOOO
; 84B0 17 01 E7 8D B6 27 0D EC C4 33 CB 11 93 1C 25 E8 -----'---3----%- OOOOOOOOOOOOOOOO
; 84C0 1A 04 35 C0 17 01 D3 17 01 C7 1F 30 1C FB 35 C0 --5--------0--5- OOOOOOOOOOOOOOOO
; 84D0 81 61 25 06 81 7A 22 02 80 20 81 5F 27 10 81 41 -a%--z"-- -_'--A OOOOOOOOOOOOOOOO
; 84E0 25 0F 81 5A 23 08 81 61 25 07 81 7A 22 03 1A 04 %--Z#--a%--z"--- OOOOOOOOOOOOOOOO
; 84F0 39 1C FB 39 0F 42 34 06 17 01 9F 17 01 93 8D 2B 9--9-B4--------+ OOOOOOOOOOOOOOOO
; 8500 81 0D 27 21 81 4C 26 07 36 10 17 01 B5 20 16 81 --'!-L&-6---- -- OOOOOOOOOOOOOOOO
; 8510 23 26 04 8D 2B 20 E1 81 24 10 26 0D 6A 8D 34 81 #&--+ --$-&-j-4- OOOOOOOOOOOOOOOO
; 8520 24 27 FA 20 DB D6 42 36 04 35 86 17 01 6C 34 02 $'- --B6-5---l4- OOOOOOOOOOOOOOOO
; 8530 81 0D 27 0A 0C 42 31 21 A6 A4 8D 9E 27 F8 35 82 --'--B1!----'-5- OOOOOOOOOOOOOOOO
; 8540 34 06 A6 84 81 23 26 04 8D 69 20 05 17 00 87 36 4----#&--i ----6 OOOOOOOOOOOOOOOO
; 8550 06 35 86 34 04 17 01 39 81 24 26 06 8D 55 8D CB -5-4---9-$&--U-- OOOOOOOOOOOOOOOO
; 8560 20 1C 36 10 17 FB BB 30 8B 30 02 8D BE 81 24 26  -6----0-0----$& OOOOOOOOOOOOOOOO
; 8570 0D 34 12 17 01 1B 81 24 35 12 27 02 30 1F 35 84 -4-----$5-'-0-5- OOOOOOOOOOOOOOOO
; 8580 34 32 10 9E 29 C6 00 17 01 10 81 0D 27 1D 17 01 42--)-------'--- OOOOOOOOOOOOOOOO
; 8590 00 CB 01 A6 80 A1 A0 26 05 17 FE CF 27 0F AE 61 -------&----'--a OOOOOOOOOOOOOOOO
; 85A0 A6 A0 17 FF 35 27 F9 31 3F 20 DC C6 00 AF 61 C1 ----5'-1? ----a- OOOOOOOOOOOOOOOO
; 85B0 00 35 B2 34 20 A6 84 81 24 27 06 81 23 10 26 0C -5-4 ---$'--#-&- OOOOOOOOOOOOOOOO
; 85C0 C6 8D BD 10 27 0C C9 10 9E 27 50 EB 24 58 31 25 ----'----'P-$X1% OOOOOOOOOOOOOOOO
; 85D0 EC A5 36 06 35 A0 A6 84 81 2A 26 07 30 01 CC FF --6-5----*&-0--- OOOOOOOOOOOOOOOO
; 85E0 FF 20 10 CC 00 00 8D 18 26 05 CC 00 01 20 04 8D - ------&---- -- OOOOOOOOOOOOOOOO
; 85F0 0F 26 FC 39 27 10 03 E8 00 64 00 0A 00 01 00 00 -&-9'----d------ OOOOOOOOOOOOOOOO
; 8600 34 26 E6 84 C0 30 C1 0A 24 18 30 01 86 00 10 8E 4&---0--$-0----- OOOOOOOOOOOOOOOO
; 8610 00 0A E3 E4 10 25 0C 4E 31 3F 26 F6 ED E4 1C FB -----%-N1?&----- OOOOOOOOOOOOOOOO
; 8620 35 A6 1A 04 35 A6 34 36 30 8D FF C8 10 8E 2F 20 5---5-460-----/  OOOOOOOOOOOOOOOO
; 8630 31 A9 01 00 A3 84 24 F8 E3 81 34 06 EC 84 1F 20 1-----$---4----  OOOOOOOOOOOOOOOO
; 8640 27 17 10 8E 2F 30 10 83 30 20 26 06 10 8E 2F 20 '---/0--0 &---/  OOOOOOOOOOOOOOOO
; 8650 1F 98 17 FA B2 35 06 20 D7 17 FA AB 32 62 35 B6 -----5- ----2b5- OOOOOOOOOOOOOOOO
; 8660 34 16 9E 27 EC 1E 35 96 34 16 0D 23 27 15 9E 27 4--'--5-4--#'--' OOOOOOOOOOOOOOOO
; 8670 EC 1E 27 0F 10 83 FF FF 26 04 1C FB 20 05 83 00 --'-----&--- --- OOOOOOOOOOOOOOOO
; 8680 01 ED 1E 35 96 34 16 9E 27 EC 1E 10 83 FF FF 35 ---5-4--'------5 OOOOOOOOOOOOOOOO
; 8690 96 A6 80 81 20 27 FA 30 1F 39 A6 A0 81 20 27 FA ---- '-0-9--- '- OOOOOOOOOOOOOOOO
; 86A0 31 3F 39 34 06 DC 1C 10 9F 1C 8D 06 34 01 DD 1C 1?94--------4--- OOOOOOOOOOOOOOOO
; 86B0 35 87 34 02 9C 1C 27 08 A6 80 81 0D 26 F6 1C FB 5-4---'-----&--- OOOOOOOOOOOOOOOO
; 86C0 35 82 9C 1C 27 08 8D EA 9C 18 27 02 30 1F 1C FB 5---'-----'-0--- OOOOOOOOOOOOOOOO
; 86D0 39 8D 04 27 14 30 1F 34 02 9C 18 27 0A A6 82 81 9--'-0-4---'---- OOOOOOOOOOOOOOOO
; 86E0 0D 26 F6 30 01 1C FB 35 02 39 34 36 CC 00 0B 10 -&-0---5-946---- OOOOOOOOOOOOOOOO
; 86F0 9E 1C 17 04 CB 30 AB 10 9F 16 9F 1C 9F 18 9F 1A -----0---------- OOOOOOOOOOOOOOOO
; 8700 ED A4 ED 22 A6 E4 A7 24 EC 64 ED 25 DC 00 DD 04 ---"---$-d-%---- OOOOOOOOOOOOOOOO
; 8710 DC 02 DD 06 35 B6 34 36 9F 1A EC 84 17 04 CD 40 ----5-46-------@ OOOOOOOOOOOOOOOO
; 8720 50 82 00 9E 1C 30 8B 9F 16 31 0B 10 9F 18 EC 02 P----0---1------ OOOOOOOOOOOOOOOO
; 8730 31 8B 10 9F 1A EC 07 DD 04 EC 09 DD 06 35 B6 34 1------------5-4 OOOOOOOOOOOOOOOO
; 8740 36 9E 16 DC 1C 93 16 ED 84 DC 1A 93 16 ED 02 DC 6--------------- OOOOOOOOOOOOOOOO
; 8750 04 ED 07 DC 06 ED 09 A6 04 81 42 26 06 EC 05 DD ----------B&---- OOOOOOOOOOOOOOOO
; 8760 24 20 0B 10 9E 14 10 9F 1A EC 84 17 04 CA 35 B6 $ ------------5- OOOOOOOOOOOOOOOO
; 8770 34 06 9E 14 A6 04 81 42 26 07 EC E4 10 A3 05 27 4------B&------' OOOOOOOOOOOOOOOO
; 8780 07 17 01 17 25 EE 1C FB 35 86 9E 12 EC 45 C3 04 ----%---5----E-- OOOOOOOOOOOOOOOO
; 8790 1F 30 8B 9C 1C 23 0A 10 3F 07 10 25 0B 0C 10 9F -0---#--?--%---- OOOOOOOOOOOOOOOO
; 87A0 1E 37 C0 17 01 7C 17 F9 56 DC 1C 93 14 17 FE 76 -7---|--V------v OOOOOOOOOOOOOOOO
; 87B0 86 20 17 F9 52 17 F9 4F DC 1E 93 14 17 FE 67 17 - --R--O------g- OOOOOOOOOOOOOOOO
; 87C0 F9 3D 16 01 64 9E 16 A6 04 81 42 10 26 0A AF DC -=--d-----B-&--- OOOOOOOOOOOOOOOO
; 87D0 1C 93 16 ED 84 EC 45 10 A3 05 27 1D 8D 92 27 13 ------E---'---'- OOOOOOOOOOOOOOOO
; 87E0 CC 00 0B 17 03 DA 17 FF 56 86 42 10 AE 45 17 FE --------V-B--E-- OOOOOOOOOOOOOOOO
; 87F0 F9 37 C0 17 FF 49 17 FF 1D 37 C0 0D 41 10 26 0A -7---I---7--A-&- OOOOOOOOOOOOOOOO
; 8800 7D 9E 16 A6 04 81 42 10 26 0A 73 AE 05 10 9E 24 }-----B-&-s----$ OOOOOOOOOOOOOOOO
; 8810 34 30 AE 45 17 FE 7A A1 01 26 0A 17 FF 21 86 4D 40-E--z--&---!-M OOOOOOOOOOOOOOOO
; 8820 17 FE C7 20 11 30 01 17 FC 77 10 27 0A 4A 17 FF --- -0---w-'-J-- OOOOOOOOOOOOOOOO
; 8830 0E 1F 01 17 FE E0 35 30 9F 24 10 9F 2D 86 4D 97 ------50-$----M- OOOOOOOOOOOOOOOO
; 8840 26 37 C0 17 00 DC 30 8D 0B 7C 17 F8 A2 9E 14 E6 &7----0--|------ OOOOOOOOOOOOOOOO
; 8850 04 C1 42 26 1E EC 05 9C 16 27 08 10 93 24 27 06 --B&-----'---$'- OOOOOOOOOOOOOOOO
; 8860 86 20 8C 86 2A 8C 86 24 17 F8 9C EC 05 17 FD B6 - --*--$-------- OOOOOOOOOOOOOOOO
; 8870 17 F8 8C 8D 26 25 D8 30 8D 0B 43 17 F8 71 9E 14 ----&%-0--C--q-- OOOOOOOOOOOOOOOO
; 8880 34 10 A6 04 81 4D 26 0B 30 0B 17 F7 FE 35 10 8D 4----M&-0----5-- OOOOOOOOOOOOOOOO
; 8890 0A 25 ED 17 F8 69 17 00 90 37 C0 34 06 EC 84 30 -%---i---7-4---0 OOOOOOOOOOOOOOOO
; 88A0 8B 9C 1C 35 86 0D 41 10 26 09 D3 AE 45 A6 80 34 ---5--A-&---E--4 OOOOOOOOOOOOOOOO
; 88B0 02 17 FD DD 17 FB EA 10 27 09 BD 1F 02 EC A4 9E --------'------- OOOOOOOOOOOOOOOO
; 88C0 1A 34 16 10 9F 1A 17 03 23 DC 16 A3 E4 DD 16 DC -4------#------- OOOOOOOOOOOOOOOO
; 88D0 18 A3 E4 DD 18 DC 1C A3 E4 DD 1C 35 10 35 06 34 -----------5-5-4 OOOOOOOOOOOOOOOO
; 88E0 10 A3 E1 DD 1A 37 C0 34 40 AE 45 17 F8 42 1F 02 -----7-4@-E--B-- OOOOOOOOOOOOOOOO
; 88F0 1F 13 30 8C 17 86 01 C6 00 10 3F 03 10 25 09 AA --0-------?--%-- OOOOOOOOOOOOOOOO
; 8900 10 3F 04 5D 10 26 09 A2 35 40 37 C0 53 48 45 4C -?-]-&--5@7-SHEL OOOOOOOOOOOOOOOO
; 8910 4C 0D EC 45 27 02 86 01 97 3E 0D 41 26 02 97 40 L--E'---->-A&--@ OOOOOOOOOOOOOOOO
; 8920 37 C0 96 3E 97 3F 0C 3E 39 96 3F 97 3E 39 AE C4 7-->-?->9-?->9-- OOOOOOOOOOOOOOOO
; 8930 AE 02 10 9E 45 10 9C 49 10 23 09 51 AF A3 8E 00 ----E--I-#-Q---- OOOOOOOOOOOOOOOO
; 8940 00 AF A3 10 9F 45 37 C0 9E 45 EC 84 C3 00 01 ED -----E7--E------ OOOOOOOOOOOOOOOO
; 8950 84 10 A3 45 25 04 8D 0C 37 C0 10 AE 02 AE C4 10 ---E%---7------- OOOOOOOOOOOOOOOO
; 8960 AF 02 37 C0 34 10 9E 45 30 04 9C 43 10 22 09 1A --7-4--E0--C-"-- OOOOOOOOOOOOOOOO
; 8970 9F 45 35 90 8D AC 9E 1A 17 FC E5 27 0D 17 F7 0B -E5--------'---- OOOOOOOOOOOOOOOO
; 8980 17 FD 2F 27 05 17 FC E0 26 F3 8D 9D 37 C0 8D 92 --/'----&---7--- OOOOOOOOOOOOOOOO
; 8990 17 FC CD 27 1A 9E 1A 17 FD 3D 20 05 17 FD 32 27 ---'-----= ---2' OOOOOOOOOOOOOOOO
; 89A0 05 17 FC C4 26 F6 34 10 DC 1A A3 E1 17 F6 E5 17 ----&-4--------- OOOOOOOOOOOOOOOO
; 89B0 FF 77 37 C0 0C 2F EC 45 DD 30 26 02 0F 2F 37 C0 -w7--/-E-0&--/7- OOOOOOOOOOOOOOOO
; 89C0 17 FC EF 27 0F 34 06 DC 30 17 01 35 35 06 26 F0 ---'-4--0--55-&- OOOOOOOOOOOOOOOO
; 89D0 9C 1A 25 EC 39 34 30 9E 10 AF 62 35 90 34 26 9E --%-940---b5-4&- OOOOOOOOOOOOOOOO
; 89E0 1A A6 A0 0D 2F 27 02 8D DC 34 30 A1 A4 27 1C E6 ----/'---40--'-- OOOOOOOOOOOOOOOO
; 89F0 A0 9C 1C 24 10 E1 80 27 F2 35 30 30 01 0D 2F 27 ---$---'-500--/' OOOOOOOOOOOOOOOO
; 8A00 E8 8D BD 20 E4 1A 01 20 02 1C FE 35 30 35 A6 17 --- --- ---505-- OOOOOOOOOOOOOOOO
; 8A10 07 14 0D 41 26 13 0D 3D 27 0F 0C 39 17 FF 03 30 ---A&--='--9---0 OOOOOOOOOOOOOOOO
; 8A20 8D 08 F5 17 F6 C9 17 FF 00 37 C0 34 36 17 FC 30 ---------7-46--0 OOOOOOOOOOOOOOOO
; 8A30 1C FE 27 1A AE 45 17 F6 E9 10 AE 45 8D 97 25 0E --'--E-----E--%- OOOOOOOOOOOOOOOO
; 8A40 17 F6 3F 30 8B 9F 1A 17 FC 1E 26 ED 1C FE 35 B6 --?0------&---5- OOOOOOOOOOOOOOOO
; 8A50 34 36 17 FC 0B 1C FE 27 3B AE 45 17 F6 C4 34 06 46-----';-E---4- OOOOOOOOOOOOOOOO
; 8A60 AE 47 17 F6 BD 34 06 EC 62 A3 E4 17 01 52 10 AE -G---4--b----R-- OOOOOOOOOOOOOOOO
; 8A70 47 17 FF 61 25 1C 9F 1A EC E4 17 05 92 AE 45 30 G--a%---------E0 OOOOOOOOOOOOOOOO
; 8A80 01 EC 62 17 01 5D 9E 1A 17 F5 F7 17 FB DA 26 D7 --b--]--------&- OOOOOOOOOOOOOOOO
; 8A90 1C FE 32 64 35 B6 17 FF 92 10 25 FF 72 39 17 FF --2d5-----%-r9-- OOOOOOOOOOOOOOOO
; 8AA0 AF 10 25 FF 6A 39 9E 1A 17 FF A5 20 05 9E 1A 17 --%-j9----- ---- OOOOOOOOOOOOOOOO
; 8AB0 FF 79 34 01 9C 1A 27 07 9E 1A 17 FC 1A 9F 1A 35 -y4---'--------5 OOOOOOOOOOOOOOOO
; 8AC0 01 10 25 FF 4A 39 17 FB 97 27 34 AE 45 17 F6 52 --%-J9---'4-E--R OOOOOOOOOOOOOOOO
; 8AD0 30 01 34 16 9E 1A 9C 1C 25 05 17 06 49 20 20 17 0-4-----%---I  - OOOOOOOOOOOOOOOO
; 8AE0 FB E0 EC E4 17 00 D9 9F 1A AE 62 17 00 F5 9E 1A ----------b----- OOOOOOOOOOOOOOOO
; 8AF0 17 F5 8F 17 FB BC 9F 1A 35 16 17 FB 6B 26 D3 37 --------5---k&-7 OOOOOOOOOOOOOOOO
; 8B00 C0 34 20 10 83 00 00 27 1D 1F 02 17 FB C9 A6 84 -4 ----'-------- OOOOOOOOOOOOOOOO
; 8B10 81 0D 27 0C 9C 1C 24 08 30 01 31 3F 26 F0 30 1F --'---$-0-1?&-0- OOOOOOOOOOOOOOOO
; 8B20 1F 20 10 83 00 00 35 A0 17 FB 35 27 2F 9E 1A EC - ----5---5'/--- OOOOOOOOOOOOOOOO
; 8B30 45 8D CE 9F 1A ED 45 27 23 1F 02 17 FB 2A 27 1C E-----E'#----*'- OOOOOOOOOOOOOOOO
; 8B40 31 3F 86 20 9E 1C 34 10 9C 1E 10 24 07 21 A7 80 1?- --4----$-!-- OOOOOOOOOOOOOOOO
; 8B50 17 FB 15 26 F3 1F 20 35 10 17 00 87 37 C0 17 FA ---&-- 5----7--- OOOOOOOOOOOOOOOO
; 8B60 FF 27 31 9E 1A 17 FB 6F 34 10 AE 45 17 F5 B3 30 -'1----o4--E---0 OOOOOOOOOOOOOOOO
; 8B70 01 10 9E 1C 8D 20 10 27 06 F5 17 FA EB 26 F5 9E ----- -'-----&-- OOOOOOOOOOOOOOOO
; 8B80 1C 1F 20 93 1C 8D 5C 86 0D 8D 2A AE E4 DC 1A A3 -- ---\---*----- OOOOOOOOOOOOOOOO
; 8B90 E1 17 F5 00 37 C0 34 56 1F 03 10 9C 1E 24 12 11 ----7-4V-----$-- OOOOOOOOOOOOOOOO
; 8BA0 83 00 00 27 08 A6 80 A7 A0 33 5F 20 ED 1C FB 35 ---'-----3_ ---5 OOOOOOOOOOOOOOOO
; 8BB0 D6 1A 04 35 D6 34 16 1F 41 CC 00 01 8D 25 35 96 ---5-4--A----%5- OOOOOOOOOOOOOOOO
; 8BC0 34 10 9E 1C 30 8B 9C 1E 10 24 06 A3 35 90 34 20 4---0----$--5-4  OOOOOOOOOOOOOOOO
; 8BD0 10 9E 1C 8D C1 10 27 06 96 10 9F 1C 35 A0 86 0D ------'-----5--- OOOOOOOOOOOOOOOO
; 8BE0 17 F5 4F 8D E9 8D 51 D3 1A DD 1A 39 34 76 10 83 --O---Q----94v-- OOOOOOOOOOOOOOOO
; 8BF0 00 00 27 42 DD CF DC 1A 93 1C 1F 02 E3 E4 DD D1 --'B------------ OOOOOOOOOOOOOOOO
; 8C00 DC 1C 93 CF DD D3 9E 1C A6 82 9F D5 97 CD 20 12 -------------- - OOOOOOOOOOOOOOOO
; 8C10 9C D5 26 0A 96 CD A7 C4 A6 82 9F D5 97 CD 31 21 --&-----------1! OOOOOOOOOOOOOOOO
; 8C20 27 14 DC D1 1F 13 30 8B A6 84 A7 C4 9C D3 24 E0 '-----0-------$- OOOOOOOOOOOOOOOO
; 8C30 DC CF 31 21 26 EE 35 F6 34 06 DC 1C 93 1A A3 E4 --1!&-5-4------- OOOOOOOOOOOOOOOO
; 8C40 8D AA 35 86 17 FA 19 27 66 DC 24 17 FB 22 34 10 --5----'f-$--"4- OOOOOOOOOOOOOOOO
; 8C50 EC 84 31 8B 30 0B 34 10 34 20 AC E1 26 05 17 04 --1-0-4-4 --&--- OOOOOOOOOOOOOOOO
; 8C60 C5 20 0A 17 FA 3D 27 05 17 F9 FD 26 EB 1F 10 A3 - ---='----&---- OOOOOOOOOOOOOOOO
; 8C70 E4 35 30 34 06 17 F4 1C EC 22 83 00 0B A3 E4 24 -504-----"-----$ OOOOOOOOOOOOOOOO
; 8C80 03 CC 00 00 C3 00 0B ED 22 EC A4 A3 E4 ED A4 DC --------"------- OOOOOOOOOOOOOOOO
; 8C90 16 A3 E4 DD 16 DC 18 A3 E4 DD 18 35 06 10 9E 1A -----------5---- OOOOOOOOOOOOOOOO
; 8CA0 9F 1A 9E 1C 10 9F 1C 17 FF 42 9F 1C 10 9F 1A 37 ---------B-----7 OOOOOOOOOOOOOOOO
; 8CB0 C0 17 F9 AC 27 57 9E 1C 34 10 DC 24 17 FA B1 34 ----'W--4--$---4 OOOOOOOOOOOOOOOO
; 8CC0 10 9E 1A 34 10 9C 1C 25 05 17 04 5A 20 0A 17 F9 ---4---%---Z --- OOOOOOOOOOOOOOOO
; 8CD0 E1 27 05 17 F9 92 26 ED 1F 10 A3 E4 35 30 17 F3 -'----&-----50-- OOOOOOOOOOOOOOOO
; 8CE0 B3 30 8B 34 16 9F 1C EC A4 E3 E4 ED A4 EC 22 30 -0-4----------"0 OOOOOOOOOOOOOOOO
; 8CF0 AB 9F 1A E3 E4 ED 22 DC 16 E3 E4 DD 16 DC 18 E3 ------"--------- OOOOOOOOOOOOOOOO
; 8D00 E4 DD 18 35 36 17 FF 30 9F 1A 10 9F 1C 37 C0 0D ---56--0-----7-- OOOOOOOOOOOOOOOO
; 8D10 05 27 3A 17 F9 4A 27 62 9E 1C 31 89 00 80 10 9C -':--J'b--1----- OOOOOOOOOOOOOOOO
; 8D20 1E 23 05 8D 43 16 05 47 96 04 10 8E 00 80 10 3F -#--C--G-------? OOOOOOOOOOOOOOOO
; 8D30 8B 24 2C 34 04 8D 31 35 04 C1 D3 10 26 05 6B DC -$,4--15----&-k- OOOOOOOOOOOOOOOO
; 8D40 04 10 93 00 27 07 10 3F 8F 10 25 05 5D 0F 05 17 ----'--?--%-]--- OOOOOOOOOOOOOOOO
; 8D50 03 D4 0D 41 26 07 30 8D 06 30 17 F3 92 20 1B 1F ---A&-0--0--- -- OOOOOOOOOOOOOOOO
; 8D60 20 30 8B 17 F9 02 26 B2 1F 10 93 1C 9E 1A 34 16  0----&-------4- OOOOOOOOOOOOOOOO
; 8D70 9E 1C 17 FE 6E 35 16 17 F3 1A 39 0D 07 10 27 04 ----n5----9---'- OOOOOOOOOOOOOOOO
; 8D80 F4 17 F8 DC 27 2F 10 9E 1A 1F 21 17 F3 A2 26 0D ----'/----!---&- OOOOOOOOOOOOOOOO
; 8D90 8D 17 17 03 91 30 8D 05 E3 17 F3 53 39 31 8B 96 -----0-----S91-- OOOOOOOOOOOOOOOO
; 8DA0 06 17 F3 24 17 F8 C1 26 E0 9E 1A 1F 20 93 1A 17 ---$---&---- --- OOOOOOOOOOOOOOOO
; 8DB0 F2 E2 17 02 5A 39 DC 04 10 93 00 27 0C 5D 27 09 ----Z9-----'-]'- OOOOOOOOOOOOOOOO
; 8DC0 10 3F 8F 10 25 04 E3 0F 05 AE 45 E6 80 E1 84 26 -?--%-----E----& OOOOOOOOOOOOOOOO
; 8DD0 06 DC 00 DD 04 37 C0 17 00 EC 10 26 04 CA 86 01 -----7-----&---- OOOOOOOOOOOOOOOO
; 8DE0 10 3F 84 10 25 04 C3 C6 01 DD 04 37 C0 DC 06 10 -?--%------7---- OOOOOOOOOOOOOOOO
; 8DF0 93 02 27 0C 5D 27 09 10 3F 8F 10 25 04 AC 0F 07 --'-]'--?--%---- OOOOOOOOOOOOOOOO
; 8E00 AE 45 E6 80 E1 84 26 06 DC 02 DD 06 37 C0 17 00 -E----&-----7--- OOOOOOOOOOOOOOOO
; 8E10 B5 10 26 04 93 CC 02 1B 10 3F 83 10 25 04 8B C6 --&------?--%--- OOOOOOOOOOOOOOOO
; 8E20 01 DD 06 37 C0 AE 45 E6 80 17 00 9A 10 26 04 78 ---7--E------&-x OOOOOOOOOOOOOOOO
; 8E30 86 01 10 3F 84 10 25 04 71 97 08 9E 1C 10 8E 00 ---?--%-q------- OOOOOOOOOOOOOOOO
; 8E40 0B 8D 74 96 08 10 3F 89 25 24 A6 04 81 4D 27 08 --t---?-%$---M'- OOOOOOOOOOOOOOOO
; 8E50 EC 05 27 04 C6 D3 20 16 6F 08 6F 0A EC 84 83 00 --'--- -o-o----- OOOOOOOOOOOOOOOO
; 8E60 0B 1F 02 30 0B 8D 50 96 08 10 3F 89 24 11 34 04 ---0--P---?-$-4- OOOOOOOOOOOOOOOO
; 8E70 96 08 10 3F 8F 35 04 C1 D3 10 26 04 2D 37 C0 17 ---?-5----&--7-- OOOOOOOOOOOOOOOO
; 8E80 F8 0F 34 10 17 F6 1A 35 10 26 B0 17 F1 FD 9E 1C --4----5-&------ OOOOOOOOOOOOOOOO
; 8E90 EC 84 10 9E 1A 30 8B 9F 1C 34 26 9E 14 9F 1A 17 -----0---4&----- OOOOOOOOOOOOOOOO
; 8EA0 FD 96 DC 16 E3 E4 DD 16 DC 18 E3 E4 DD 18 35 26 --------------5& OOOOOOOOOOOOOOOO
; 8EB0 31 AB 10 9F 1A 20 84 34 26 1F 20 31 8B 10 9C 1E 1---- -4&- 1---- OOOOOOOOOOOOOOOO
; 8EC0 10 24 03 AB 35 A6 17 F7 C8 34 30 31 84 E1 A0 26 -$--5----401---& OOOOOOOOOOOOOOOO
; 8ED0 FC 34 20 17 F2 84 25 09 10 AC E1 24 04 1A 04 35 -4 ---%----$---5 OOOOOOOOOOOOOOOO
; 8EE0 B0 1C FB 35 B0 8E FF FF 34 10 AE 47 30 01 17 F7 ---5----4--G0--- OOOOOOOOOOOOOOOO
; 8EF0 A0 17 F5 AD 10 27 03 80 34 06 17 F7 94 10 AE 47 -----'--4------G OOOOOOOOOOOOOOOO
; 8F00 A1 A4 26 ED AE 45 E6 80 8D BC 10 26 03 9A CC 02 --&--E-----&---- OOOOOOOOOOOOOOOO
; 8F10 1B 10 3F 83 10 25 03 92 97 08 35 10 8C FF FF 27 --?--%----5----' OOOOOOOOOOOOOOOO
; 8F20 1B CC 00 00 ED 05 10 AE 84 96 08 10 3F 8A 24 EA ------------?-$- OOOOOOOOOOOOOOOO
; 8F30 34 04 96 08 10 3F 8F 35 04 16 03 6E 96 08 10 3F 4----?-5---n---? OOOOOOOOOOOOOOOO
; 8F40 8F 37 C0 0D 01 27 47 9E 1C 31 8B 31 A9 00 80 10 -7---'G--1-1---- OOOOOOOOOOOOOOOO
; 8F50 9C 1E 25 09 10 83 00 80 25 34 83 00 80 1F 02 96 --%-----%4------ OOOOOOOOOOOOOOOO
; 8F60 00 10 3F 89 25 11 1F 20 30 8B 9F 1C 10 8E 00 80 --?-%-- 0------- OOOOOOOOOOOOOOOO
; 8F70 96 00 10 3F 8B 24 11 C1 D3 10 26 03 2D 30 8D 04 ---?-$----&--0-- OOOOOOOOOOOOOOOO
; 8F80 09 17 F1 6B 0F 05 20 06 1F 20 30 8B 9F 1C 39 0D ---k-- -- 0---9- OOOOOOOOOOOOOOOO
; 8F90 03 27 15 9E 18 DC 1A 93 18 1F 02 96 02 10 3F 8A -'------------?- OOOOOOOOOOOOOOOO
; 8FA0 9F 1A 1F 20 8D 69 8D 9B 37 C0 17 F6 B3 27 25 9E --- -i--7----'%- OOOOOOOOOOOOOOOO
; 8FB0 1A 17 F7 23 9F 1A 34 10 9C 1C 26 05 17 01 67 20 ---#--4---&---g  OOOOOOOOOOOOOOOO
; 8FC0 0D 17 F0 C7 17 F6 EB 27 05 17 F6 9C 26 EA 1F 10 -------'----&--- OOOOOOOOOOOOOOOO
; 8FD0 A3 E1 8D 3B 37 C0 17 F6 87 27 1F 9E 1A 34 10 9C ---;7----'---4-- OOOOOOOOOOOOOOOO
; 8FE0 1C 26 05 17 01 40 20 07 30 01 17 F6 7B 26 F0 1F -&---@ -0---{&-- OOOOOOOOOOOOOOOO
; 8FF0 10 A3 E4 35 10 17 F0 9C 8D 15 37 C0 9E 1A 17 F1 ---5------7----- OOOOOOOOOOOOOOOO
; 9000 2F 27 0A 83 00 01 27 05 8D 05 17 F0 75 37 C0 34 /'----'-----u7-4 OOOOOOOOOOOOOOOO
; 9010 06 17 FB D8 DC 1C A3 E4 DD 1C 35 86 17 F6 41 27 ----------5---A' OOOOOOOOOOOOOOOO
; 9020 14 9E 1A 9C 18 26 05 17 00 FC 20 07 30 1F 17 F6 -----&---- -0--- OOOOOOOOOOOOOOOO
; 9030 37 26 F0 9F 1A 37 C0 17 F6 26 27 F9 9E 1A 9C 1C 7&---7---&'----- OOOOOOOOOOOOOOOO
; 9040 26 05 17 00 E1 20 EC 30 01 17 F6 1C 26 F0 20 E3 &---- -0----&- - OOOOOOOOOOOOOOOO
; 9050 9E 1A 17 F6 0B 26 05 17 F6 68 20 13 9C 1C 26 05 -----&---h ---&- OOOOOOOOOOOOOOOO
; 9060 17 00 C3 20 0A 17 F6 4A 27 05 17 F5 FB 26 ED 9F --- ---J'----&-- OOOOOOOOOOOOOOOO
; 9070 1A 17 F0 17 37 C0 9E 1A 17 F5 E5 26 05 17 F6 57 ----7------&---W OOOOOOOOOOOOOOOO
; 9080 20 ED 9C 18 26 05 17 00 9D 20 E4 17 F6 43 27 DF  ---&---- ---C'- OOOOOOOOOOOOOOOO
; 9090 17 F5 D5 26 ED 20 D8 9E 18 9F 1A 37 C0 9E 1C 9F ---&- -----7---- OOOOOOOOOOOOOOOO
; 90A0 1A 37 C0 86 01 97 3C 0F 3A 37 C0 86 01 97 3D 0F -7----<-:7----=- OOOOOOOOOOOOOOOO
; 90B0 3A 37 C0 4F 97 3D 97 3A 37 C0 9E 1A 9C 1C 27 EB :7-O-=-:7-----'- OOOOOOOOOOOOOOOO
; 90C0 A6 84 81 0D 27 E5 20 EB 9E 1A 9C 1C 27 E5 A6 84 ----'- -----'--- OOOOOOOOOOOOOOOO
; 90D0 81 0D 27 DF 20 D5 9E 1A 9C 1C 26 D7 20 CD 9E 1A --'- -----&- --- OOOOOOOOOOOOOOOO
; 90E0 9C 1C 27 CF 20 C5 8D 0A 26 C1 20 C7 8D 04 27 BB --'- ---&- ---'- OOOOOOOOOOOOOOOO
; 90F0 20 C1 AE 45 10 9E 1A E6 80 E1 84 27 0D 10 9C 1C  --E-------'---- OOOOOOOOOOOOOOOO
; 9100 27 06 A6 80 A1 A0 27 F1 1C FB 39 0D 05 26 A4 20 '-----'---9--&-  OOOOOOOOOOOOOOOO
; 9110 9A 96 05 27 9E 20 94 EC 45 27 98 20 8E 86 00 17 ---'- --E'- ---- OOOOOOOOOOOOOOOO
; 9120 F5 63 27 8F 20 85 34 02 17 F5 5A 27 06 86 01 97 -c'- -4---Z'---- OOOOOOOOOOOOOOOO
; 9130 3D 0F 3A 35 82 86 00 97 3D 97 3A 4C 97 3B 37 C0 =-:5----=-:L-;7- OOOOOOOOOOOOOOOO
; 9140 86 00 97 3A 4C 97 3D 97 3B 37 C0 37 C0 0D 41 10 ---:L-=-;7-7--A- OOOOOOOOOOOOOOOO
; 9150 26 01 2B 9E 16 A6 04 81 42 27 6A 9E 1C 9C 18 10 &-+-----B'j----- OOOOOOOOOOOOOOOO
; 9160 27 01 1E 86 0D A1 1F 27 0C 30 01 9C 1E 10 24 00 '------'-0----$- OOOOOOOOOOOOOOOO
; 9170 FE A7 1F 9F 1C 9E 18 17 F5 17 17 F3 5D 10 26 01 ------------]-&- OOOOOOOOOOOOOOOO
; 9180 00 34 10 A6 80 17 F3 52 27 F9 81 20 27 0E 81 0D -4-----R'-- '--- OOOOOOOOOOOOOOOO
; 9190 27 0A 81 24 27 06 81 23 10 26 00 E5 AE E4 17 F3 '--$'--#-&------ OOOOOOOOOOOOOOOO
; 91A0 00 10 26 00 DF C6 2E 35 10 17 F2 69 10 27 00 D4 --&---.5---i-'-- OOOOOOOOOOOOOOOO
; 91B0 17 F5 8C DC 24 17 F5 B8 17 F5 5B DC 2D DD 24 86 ----$-----[---$- OOOOOOOOOOOOOOOO
; 91C0 45 97 26 37 C0 CC 00 01 17 F5 A5 9C 16 27 03 16 E-&7---------'-- OOOOOOOOOOOOOOOO
; 91D0 00 9A 10 9E 12 31 A9 00 89 AE A1 27 07 AD 84 10 -----1-----'---- OOOOOOOOOOOOOOOO
; 91E0 3F 02 20 F5 9E 18 DC 1C 93 18 1F 02 DC 02 C1 00 ?- ------------- OOOOOOOOOOOOOOOO
; 91F0 27 6F 10 3F 8A 25 6C 9E 14 DC 1E 93 14 1F 02 DC 'o-?-%l--------- OOOOOOOOOOOOOOOO
; 9200 00 C1 00 27 5C 10 3F 89 24 E2 C1 D3 26 55 0D 36 ---'\-?-$---&U-6 OOOOOOOOOOOOOOOO
; 9210 27 4F DC 1E 93 14 10 3F 07 25 48 96 00 10 3F 8F 'O-----?-%H---?- OOOOOOOOOOOOOOOO
; 9220 25 41 96 02 10 3F 8F 25 3A 9E 32 10 3F 87 25 31 %A---?-%:-2-?-%1 OOOOOOOOOOOOOOOO
; 9230 10 9E 32 9E 32 A6 A0 A7 80 81 2F 27 F6 81 0D 26 --2-2-----/'---& OOOOOOOOOOOOOOOO
; 9240 F4 10 9E 12 30 A9 00 FE 1F 13 17 EE E3 30 A9 00 ----0--------0-- OOOOOOOOOOOOOOOO
; 9250 F7 1F 02 CC 01 00 10 3F 03 25 08 10 3F 04 5D 26 -------?-%--?-]& OOOOOOOOOOOOOOOO
; 9260 02 C6 00 10 3F 06 C6 00 8C C6 0B 8C C6 13 8C C6 ----?----------- OOOOOOOOOOOOOOOO
; 9270 22 8C C6 33 8C C6 41 8C C6 4F 8C C6 59 8C C6 5F "--3--A--O--Y--_ OOOOOOOOOOOOOOOO
; 9280 8C C6 6D 8C C6 7C 8C C6 87 8C C6 94 8C C6 94 8C --m--|---------- OOOOOOOOOOOOOOOO
; 9290 C6 A5 86 FF 97 23 0C 3E 30 8D 00 2D 4F 30 8B 17 -----#->0---O0-- OOOOOOOOOOOOOOOO
; 92A0 EE 4D 17 EE 5A 16 EF EA C6 D7 0C 3E 34 04 30 8C -M--Z------>4-0- OOOOOOOOOOOOOOOO
; 92B0 14 10 8E 00 04 96 0B 10 3F 8A 35 04 10 3F 0F 17 --------?-5--?-- OOOOOOOOOOOOOOOO
; 92C0 EE 3D 16 EF CD 4F 53 39 20 42 41 44 20 4E 55 4D -=---OS9 BAD NUM OOOOODDDDDDDDDDD
; 92D0 42 45 52 0D 57 48 41 54 20 3F 3F 0D 2A 20 4E 4F BER-WHAT ??-* NO DDDDDDDDDDDDDDDD
; 92E0 54 20 42 55 46 20 23 31 20 2A 0D 2A 57 4F 52 4B T BUF #1 *-*WORK DDDDDDDDDDDDDDDD
; 92F0 53 50 41 43 45 20 46 55 4C 4C 2A 0D 4D 49 53 53 SPACE FULL*-MISS DDDDDDDDDDDDDDDD
; 9300 49 4E 47 20 44 45 4C 49 4D 0D 2A 46 49 4C 45 20 ING DELIM-*FILE  DDDDDDDDDDDDDDDD
; 9310 43 4C 4F 53 45 44 2A 0D 4E 4F 54 20 46 4F 55 4E CLOSED*-NOT FOUN DDDDDDDDDDDDDDDD
; 9320 44 0D 42 52 45 41 4B 0D 4D 41 43 52 4F 20 49 53 D-BREAK-MACRO IS DDDDDDDDDDDDDDDD
; 9330 20 4F 50 45 4E 0D 42 41 44 20 4D 41 43 52 4F 20  OPEN-BAD MACRO  DDDDDDDDDDDDDDDD
; 9340 4E 41 4D 45 0D 44 55 50 4C 20 4D 41 43 52 4F 0D NAME-DUPL MACRO- DDDDDDDDDDDDDDDD
; 9350 42 41 44 20 56 41 52 20 4C 49 53 54 0D 42 52 41 BAD VAR LIST-BRA DDDDDDDDDDDDDDDD
; 9360 43 4B 45 54 20 4D 49 53 4D 41 54 43 48 0D 55 4E CKET MISMATCH-UN DDDDDDDDDDDDDDDD
; 9370 44 45 46 49 4E 45 44 20 56 41 52 0D 2A 45 4E 44 DEFINED VAR-*END DDDDDDDDDDDDDDDD
; 9380 20 4F 46 20 54 45 58 54 2A 0D 2A 45 4E 44 20 4F  OF TEXT*-*END O DDDDDDDDDDDDDDDD
; 9390 46 20 46 49 4C 45 2A 0D 2A 46 41 49 4C 2A 0D 52 F FILE*-*FAIL*-R DDDDDDDDDDDDDDDD
; 93A0 45 4E 41 4D 45 20 0D 53 43 52 41 54 43 48 20 0D ENAME -SCRATCH - DDDDDDDDDDDDDDDD
; 93B0 45 44 54 50 32 0D 45 44 54 4C 49 42 31 0D 4D 41 EDTP2-EDTLIB1-MA DDDDDDDDDDDDDDDD
; 93C0 43 52 4F 53 3A 0D 42 55 46 46 45 52 53 3A 0D 4D CROS:-BUFFERS:-M DDDDDDDDDDDDDDDD
; 93D0 41 43 24 0D 07 FB 45 4F 46 0D 11 11 4E 45 4F 46 AC$---EOF---NEOF DDDDDDDDDDDDDDDD
; 93E0 0D 11 0B 45 4F 42 0D 10 DE 4E 45 4F 42 0D 10 D6 ---EOB---NEOB--- DDDDDDDDDDDDDDDD
; 93F0 45 4F 4C 0D 10 C8 4E 45 4F 4C 0D 10 BA 5A 45 52 EOL---NEOL---ZER DDDDDDDDDDDDDDDD
; 9400 4F 23 0D 11 17 53 54 41 52 23 0D 11 1D 53 54 52 O#---STAR#---STR DDDDDDDDDDDDDDDD
; 9410 24 0D 10 E6 4E 53 54 52 24 0D 10 EC 44 49 52 0D $---NSTR$---DIR- DDDDDDDDDDDDDDDD
; 9420 08 43 53 0D 11 35 46 0D 11 40 53 45 41 52 43 48 -CS--5F--@SEARCH DDDDDDDDDDDDDDDD
; 9430 23 24 0D 0A 96 43 48 41 4E 47 45 23 24 24 0D 0A #$---CHANGE#$$-- DDDDDDDDDDDDDDDD
; 9440 9E 4C 4F 41 44 24 0D 0E 25 53 41 56 45 24 24 0D -LOAD$--%SAVE$$- DDDDDDDDDDDDDDDD
; 9450 0E E5 53 49 5A 45 0D 07 A3 44 45 4C 24 0D 08 A5 --SIZE---DEL$--- DDDDDDDDDDDDDDDD
; 9460 52 45 41 44 24 0D 0D B6 57 52 49 54 45 24 0D 0D READ$---WRITE$-- DDDDDDDDDDDDDDDD
; 9470 ED 53 48 45 4C 4C 20 4C 0D 08 E7 4E 45 57 0D 0F -SHELL L---NEW-- DDDDDDDDDDDDDDDD
; 9480 8F 00 41 23 0D 09 B4 4C 23 0D 09 74 58 23 0D 09 --A#---L#--tX#-- DDDDDDDDDDDDDDDD
; 9490 8E 2B 23 0D 10 50 2D 23 0D 10 76 44 23 0D 0F AA -+#--P-#--vD#--- DDDDDDDDDDDDDDDD
; 94A0 45 23 24 0D 0A C6 3C 23 0D 10 1C 3E 23 0D 10 37 E#$---<#--->#--7 DDDDDDDDDDDDDDDD
; 94B0 49 23 24 0D 0B 5E 4B 23 0D 0F D6 53 23 24 0D 0A I#$--^K#---S#$-- DDDDDDDDDDDDDDDD
; 94C0 AD 43 23 24 24 0D 0A A6 55 0D 0F FC 54 23 0D 0B -C#$$---U---T#-- DDDDDDDDDDDDDDDD
; 94D0 28 42 23 0D 07 C5 5E 0D 10 97 2F 0D 10 9D 4D 23 (B#---^---/---M# DDDDDDDDDDDDDDDD
; 94E0 0D 07 8A 56 23 0D 09 12 47 23 0D 0C 44 50 23 0D ---V#---G#--DP#- DDDDDDDDDDDDDDDD
; 94F0 0C B1 5B 0D 09 2E 5D 23 0D 09 48 3A 0D 10 A3 52 --[--.]#--H:---R DDDDDDDDDDDDDDDD
; 9500 23 0D 0D 0F 57 23 0D 0D 7B 21 4C 0D 11 4B 51 0D #---W#--{!L--KQ- DDDDDDDDDDDDDDDD
; 9510 11 4D 00 04 65 2B 00 00 00 00 00 00 00 00 00 00 -M--e+---------- DDDUUUUUUUUUUUUU

 ifeq 1
Begin:Strings discovered
string $800D 3 ; "Edi"
string $8012 16 ; "(C)1981Microware"
string $8080 3 ; ")94"
string $809A 3 ; "'*1"
string $80C3 4 ; ";5 5"
string $8148 3 ; "1? "
string $8177 3 ; "4v0"
string $81EB 3 ; "n%k"
string $8220 3 ; "5 0"
string $834F 3 ; "4@0"
string $8365 3 ; "='/"
string $8379 3 ; "]&!"
string $839D 3 ; "5@5"
string $83A1 3 ; "5@5"
string $83D7 4 ; "6'&0"
string $83F5 3 ; " 5 "
string $8431 3 ; "K1$"
string $8468 5 ; "1"946"
string $8470 3 ; "i&*"
string $8535 3 ; "B1!"
string $85A7 3 ; "1? "
string $85CC 4 ; "$X1%"
string $8617 4 ; "N1?&"
string $8626 3 ; "460"
string $862E 3 ; "/ 1"
string $8648 3 ; "0 &"
string $865C 3 ; "2b5"
string $86A0 4 ; "1?94"
string $86E9 3 ; "946"
string $880F 3 ; "$40"
string $8908 3 ; "5@7"
string $890C 5 ; "SHELL"
string $89D4 3 ; "940"
string $89F9 3 ; "500"
string $8A0B 3 ; "505"
string $8A92 3 ; "2d5"
string $8ADC 3 ; "I  "
string $8B1A 3 ; "1?&"
string $8B2A 3 ; "5'/"
string $8B36 3 ; "E'#"
string $8BA9 3 ; "3_ "
string $8BEB 3 ; "94v"
string $8C1E 3 ; "1!'"
string $8C32 3 ; "1!&"
string $8C71 3 ; "504"
string $8D15 3 ; "J'b"
string $8D31 3 ; "$,4"
string $8D9B 3 ; "S91"
string $8EAE 3 ; "5&1"
string $8EC9 3 ; "401"
string $9158 3 ; "B'j"
string $920F 3 ; "6'O"
string $929B 3 ; "-O0"
string $92C5 14 ; "OS9 BAD NUMBER"
string $92D4 7 ; "WHAT ??"
string $92DC 14 ; "* NOT BUF #1 *"
string $92EB 16 ; "*WORKSPACE FULL*"
string $92FC 13 ; "MISSING DELIM"
string $930A 13 ; "*FILE CLOSED*"
string $9318 9 ; "NOT FOUND"
string $9322 5 ; "BREAK"
string $9328 13 ; "MACRO IS OPEN"
string $9336 14 ; "BAD MACRO NAME"
string $9345 10 ; "DUPL MACRO"
string $9350 12 ; "BAD VAR LIST"
string $935D 16 ; "BRACKET MISMATCH"
string $936E 13 ; "UNDEFINED VAR"
string $937C 13 ; "*END OF TEXT*"
string $938A 13 ; "*END OF FILE*"
string $9398 6 ; "*FAIL*"
string $939F 7 ; "RENAME "
string $93A7 8 ; "SCRATCH "
string $93B0 5 ; "EDTP2"
string $93B6 7 ; "EDTLIB1"
string $93BE 7 ; "MACROS:"
string $93C6 8 ; "BUFFERS:"
string $93CF 4 ; "MAC$"
string $93D6 3 ; "EOF"
string $93DC 4 ; "NEOF"
string $93E3 3 ; "EOB"
string $93E9 4 ; "NEOB"
string $93F0 3 ; "EOL"
string $93F6 4 ; "NEOL"
string $93FD 5 ; "ZERO#"
string $9405 5 ; "STAR#"
string $940D 4 ; "STR$"
string $9414 5 ; "NSTR$"
string $941C 3 ; "DIR"
string $9429 9 ; "@SEARCH#$"
string $9435 9 ; "CHANGE#$$"
string $9441 5 ; "LOAD$"
string $9448 7 ; "%SAVE$$"
string $9452 4 ; "SIZE"
string $9459 4 ; "DEL$"
string $9460 5 ; "READ$"
string $9468 6 ; "WRITE$"
string $9471 7 ; "SHELL L"
string $947B 3 ; "NEW"
string $948B 3 ; "tX#"
string $9495 3 ; "P-#"
string $949A 3 ; "vD#"
string $94A0 3 ; "E#$"
string $94AF 4 ; "7I#$"
string $94B5 3 ; "^K#"
string $94BB 3 ; "S#$"
string $94C1 4 ; "C#$$"
string $94D0 3 ; "(B#"
string $94EC 3 ; "DP#"
string $94F5 3 ; ".]#"
string $9508 3 ; "{!L"
End:Strings discovered

 endc
