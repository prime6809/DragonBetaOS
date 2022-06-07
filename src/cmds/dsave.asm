
                use     defsfile

tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     1

                mod     eom,name,tylg,atrv,start,size

; 2897

ParamCount      rmb     1		; count of parameters found on command line
u0001           rmb     1
u0002           rmb     6
u0008           rmb     2
u000A           rmb     2
u000C           rmb     2
u000E           rmb     2
u0010           rmb     6
u0016           rmb     1
u0017           rmb     1
u0018           rmb     3
u001B           rmb     6
u0021           rmb     1
u0022           rmb     1
u0023           rmb     3
u0026           rmb     6
u002C           rmb     1
u002D           rmb     208
u00FD           rmb     2644
size            equ     .


L000D           pshs    B,A
                leas    -6,S
                clrb
                clra
                std     12,Y
                std     10,Y
                std     8,Y
                std     6,Y
                std     4,Y
                std     2,Y
                std     ,Y
                clr     <41,Y
                clr     <81,Y
                clr     265,Y
                ldb     #$20
                stb     <36,Y
                leax    <36,Y
                clr     1,X
                leax    <36,Y
                clr     2,X
                leax    L0A48,PCR
                stx     585,Y
L0042           ldd     6,S
                subd    #$0001
                std     6,S
                cmpd    #$0000
                lble    L01A7

                ldx     10,S
                leax    2,X
                stx     10,S
                ldd     ,X
                std     ,S
                tfr     D,X
                ldb     ,X
                cmpb    #$2D
                lbne    L01A7

                ldd     ,S
                addd    #$0001
                std     2,S
L006C           ldb     [<2,S]
                beq     L0042

                ldb     [<2,S]
                orb     #$20
                stb     [<2,S]
                ldb     [<2,S]
                cmpb    #$62
                lbeq    L0139

                cmpb    #$69
                beq     L009B

                cmpb    #$6C
                lbeq    L0131

                cmpb    #$6D
                beq     L00B0

                cmpb    #$73
                beq     L00B8

                cmpb    #$76
                beq     L00A8

                lbra    L0187

L009B           ldd     #$0001
                std     2,Y
                clrb
                clra
                std     <16,Y
                lbra    L019D

L00A8           ldd     #$0001
                std     12,Y
                lbra    L019D

L00B0           ldd     #$0001
                std     6,Y
                lbra    L019D

L00B8           ldd     #$0001
                std     8,Y
                clrb
                clra
                std     4,S
                ldd     4,S
                addd    #$0001
                std     4,S
                subd    #$0001
                leax    <36,Y
                leax    D,X
                ldb     #$23
                stb     ,X
L00D4           ldx     2,S
                ldb     1,X
                cmpb    #$30
                blo     L0112

                ldx     2,S
                ldb     1,X
                cmpb    #$39
                bhi     L0112

                ldd     4,S
                cmpd    #$0003
                bge     L0112

                ldd     4,S
                addd    #$0001
                std     4,S
                subd    #$0001
                leax    <36,Y
                leax    D,X
                pshs    X
                ldd     4,S
                addd    #$0001
                std     4,S
                subd    #$0001
                ldx     #$0001
                leax    D,X
                ldb     ,X
                stb     [,S++]
                bra     L00D4

L0112           ldd     4,S
                addd    #$0001
                std     4,S
                subd    #$0001
                leax    <36,Y
                leax    D,X
                ldb     #$4B
                stb     ,X
                ldd     4,S
                leax    <36,Y
                leax    D,X
                clr     ,X
                lbra    L019D

L0131           ldd     #$0001
                std     4,Y
                lbra    L019D

L0139           ldx     2,S
                ldb     1,X
                cmpb    #$3D
                bne     L0180

                clrb
                clra
                std     4,S
L0145           ldd     2,S
                addd    #$0001
                std     2,S
                ldx     #$0001
                leax    D,X
                ldb     ,X
                cmpb    #$00
                beq     L016F

                ldd     4,S
                addd    #$0001
                std     4,S
                subd    #$0001
                leax    185,Y
                leax    D,X
                ldu     2,S
                ldb     u0001,U
                stb     ,X
                bra     L0145

L016F           ldd     4,S
                leax    185,Y
                leax    D,X
                clr     ,X
                ldd     #$0002
                std     ,Y
                bra     L019D

L0180           ldd     #$0001
                std     ,Y
                bra     L019D

L0187           ldb     [<2,S]
                clra
                pshs    B,A
                leax    L0A70,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    4,S
                lbsr    L09E8

L019D           ldd     2,S
                addd    #$0001
                std     2,S
                lbra    L006C

L01A7           ldd     6,S
                subd    #$0001
                std     6,S
                addd    #$0001
                cmpd    #$0000
                ble     L01E5

                ldd     [<10,S]
                std     ,S
                tfr     D,X
                ldb     ,X
                cmpb    #$2F
                beq     L01D4

                leax    L0A8C,PCR
                pshs    X
                leax    <41,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
L01D4           ldd     [<10,S]
                pshs    B,A
                leax    <41,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                bra     L01F5

L01E5           leax    L0A8E,PCR
                pshs    X
                leax    <41,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
L01F5           ldd     6,S
                subd    #$0001
                std     6,S
                addd    #$0001
                cmpd    #$0000
                ble     L023A

                ldd     #$0001
                std     10,Y
                ldx     10,S
                ldd     2,X
                std     ,S
                tfr     D,X
                ldb     ,X
                cmpb    #$2F
                beq     L0228

                leax    L0A92,PCR
                pshs    X
                leax    <81,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
L0228           ldx     10,S
                ldd     2,X
                pshs    B,A
                leax    <81,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                bra     L024A

L023A           leax    L0A94,PCR
                pshs    X
                leax    <81,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
L024A           ldd     6,S
                cmpd    #$0000
                ble     L0262

                leax    L0A98,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                lbsr    L09E8

L0262           clr     335,Y
                ldd     #$00F9
                std     <18,Y
                ldd     <18,Y
                subd    #$0001
                std     <18,Y
                addd    #$0001
                leax    335,Y
                leax    D,X
                clr     ,X
                leax    L0AB7,PCR
                tfr     X,D
                lbsr    L0D92

                ldd     10,Y
                beq     L029D

                leax    <81,Y
                pshs    X
                leax    L0ABA,PCR
                tfr     X,D
                lbsr    L0D92

                leas    2,S
L029D           leax    L0AC2,PCR
                tfr     X,D
                lbsr    L0D92

                leax    L0AD3,PCR
                tfr     X,D
                lbsr    L0D92

                ldd     12,Y
                beq     L02BC

                leax    L0ADE,PCR
                tfr     X,D
                lbsr    L0D92

L02BC           lbsr    L0840

                cmpd    #$0000
                lbne    L040C

                ldd     <22,Y
                lbsr    L0D95

                leax    <121,Y
                ldb     <29,X
                clra
                std     <26,Y
                leax    <121,Y
                ldb     <30,X
                clra
                std     <28,Y
                leax    <121,Y
                ldb     <31,X
                clra
                std     <30,Y
                leax    L0AEC,PCR
                pshs    X
                leax    L0AE9,PCR
                tfr     X,D
                lbsr    L09C2

                leas    2,S
                ldd     #$0001
                std     <24,Y
L0302           ldd     <24,Y
                lbeq    L03B2

                lbsr    L0840

                std     -2,S
                beq     L0315

                clrb
                clra
                std     <24,Y
L0315           ldd     <22,Y
                lbsr    L06BC

                cmpd    #$0000
                bge     L0333

                leax    L0AEE,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                ldd     <u0002
                lbsr    L0D98

L0333           leax    153,Y
                ldb     <31,X
                clra
                cmpd    <30,Y
                bne     L0315

                leax    153,Y
                ldb     <30,X
                clra
                cmpd    <28,Y
                bne     L0315

                leax    153,Y
                ldb     <29,X
                clra
                cmpd    <26,Y
                lbne    L0315

                leax    153,Y
                tfr     X,D
                lbsr    L07F5

                leax    153,Y
                tfr     X,D
                lbsr    L0957

                leax    L0B0C,PCR
                tfr     X,D
                lbsr    L0957

                leax    <121,Y
                ldb     <29,X
                clra
                std     <26,Y
                leax    <121,Y
                ldb     <30,X
                clra
                std     <28,Y
                leax    <121,Y
                ldb     <31,X
                clra
                std     <30,Y
                ldd     <22,Y
                lbsr    L0D95

                leax    L0B11,PCR
                pshs    X
                leax    L0B0E,PCR
                tfr     X,D
                lbsr    L09C2

                leas    2,S
                lbra    L0302

L03B2           leax    <41,Y
                tfr     X,D
                lbsr    L0957

                ldd     <18,Y
                addd    #$0001
                std     <18,Y
                leax    335,Y
                pshs    X
                addd    ,S++
                pshs    B,A
                leax    335,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                ldd     #$0001
                std     <18,Y
L03DE           ldd     <18,Y
                addd    #$0001
                std     <18,Y
                subd    #$0001
                leax    335,Y
                ldb     D,X
                cmpb    #$2F
                bne     L03DE

                leax    L0B13,PCR
                pshs    X
                ldd     <18,Y
                leax    335,Y
                pshs    X
                addd    ,S++
                lbsr    L09C2

                leas    2,S
                bra     L041C

L040C           leax    <41,Y
                pshs    X
                leax    335,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
L041C           ldd     <22,Y
                lbsr    L0D95

                clrb
                clra
                std     14,Y
                lbsr    L0450

                ldd     12,Y
                beq     L0436

                leax    L0B15,PCR
                tfr     X,D
                lbsr    L0D92

L0436           leax    L0B22,PCR
                tfr     X,D
                lbsr    L0D92

                leax    L0B2F,PCR
                tfr     X,D
                lbsr    L0D92

                clrb
                clra
                lbsr    L0D98

                leas    8,S
                rts
L0450           leas    -254,S
                ldd     14,Y
                addd    #$0001
                std     14,Y
                ldd     2,Y
                beq     L04AF

                ldd     <16,Y
                addd    #$0001
                std     <16,Y
                subd    #$0001
                leax    265,Y
                leax    D,X
                ldb     #$20
                stb     ,X
                ldd     <16,Y
                addd    #$0001
                std     <16,Y
                subd    #$0001
                leax    265,Y
                leax    D,X
                ldb     #$20
                stb     ,X
                ldd     <16,Y
                leax    265,Y
                leax    D,X
                clr     ,X
                ldd     <16,Y
                cmpd    #$001E
                ble     L04AF

                leax    L0B3F,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                lbsr    L0D98

L04AF           ldd     #$0081
                pshs    B,A
                leax    L0B65,PCR
                tfr     X,D
                lbsr    L0D9B

                leas    2,S
                std     <20,Y
                cmpd    #$0000
                bgt     L04DA

                leax    L0B67,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                ldd     <u0002
                lbsr    L0D98

L04DA           clrb
                clra
                std     <32,Y
                ldd     #$0040
                std     <34,Y
                ldd     <20,Y
                lbsr    L06F5

L04EB           ldd     <20,Y
                lbsr    L06BC

                cmpd    #$0000
                lblt    L068C

                ldd     #$0001
                pshs    B,A
                leax    153,Y
                tfr     X,D
                lbsr    L0D9B

                leas    2,S
                std     <22,Y
                cmpd    #$0000
                lbgt    L067A

                ldd     4,Y
                bne     L04EB

                ldd     #$0081
                pshs    B,A
                leax    153,Y
                tfr     X,D
                lbsr    L0D9B

                leas    2,S
                std     <22,Y
                cmpd    #$0000
                bgt     L055E

                ldd     <u0002
                pshs    B,A
                leax    153,Y
                pshs    X
                ldd     585,Y
                lbsr    L0D92

                leas    4,S
                ldd     <u0002
                pshs    B,A
                leax    153,Y
                pshs    X
                ldd     585,Y
                pshs    B,A
                ldd     <u000E
                lbsr    L0D8F

                leas    6,S
                lbra    L04EB

L055E           leax    153,Y
                tfr     X,D
                lbsr    L07F5

                ldd     6,Y
                bne     L0582

                leax    153,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0B85,PCR
                tfr     X,D
                lbsr    L0D92

                leas    4,S
L0582           ldd     <22,Y
                lbsr    L0D9E

                leax    153,Y
                tfr     X,D
                lbsr    L07F5

                leax    153,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0B92,PCR
                tfr     X,D
                lbsr    L0D92

                leas    4,S
                leax    L0B9C,PCR
                pshs    X
                leax    153,Y
                tfr     X,D
                lbsr    L09C2

                leas    2,S
                clr     ,S
                leax    335,Y
                pshs    X
                leax    2,S
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                leax    L0B9E,PCR
                pshs    X
                leax    335,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                leax    153,Y
                pshs    X
                leax    335,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                ldd     <32,Y
                std     250,S
                ldd     <34,Y
                std     252,S
                ldd     <20,Y
                lbsr    L0D9E

                lbsr    L0450

                leax    265,Y
                pshs    X
                leax    L0BA0,PCR
                tfr     X,D
                lbsr    L0D92

                leas    2,S
                leax    L0BAD,PCR
                pshs    X
                leax    L0BAA,PCR
                tfr     X,D
                lbsr    L09C2

                leas    2,S
                ldd     #$0081
                pshs    B,A
                leax    L0BAF,PCR
                tfr     X,D
                lbsr    L0D9B

                leas    2,S
                std     <20,Y
                cmpd    #$0000
                bgt     L0650

                leax    L0BB1,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                ldd     <u0002
                lbsr    L0D98

L0650           ldd     250,S
                std     <32,Y
                ldd     252,S
                std     <34,Y
                ldd     <20,Y
                lbsr    L06F5

                clr     335,Y
                leax    ,S
                pshs    X
                leax    335,Y
                tfr     X,D
                lbsr    L08EB

                leas    2,S
                lbra    L04EB

L067A           ldd     <22,Y
                lbsr    L0D9E

                leax    153,Y
                tfr     X,D
                lbsr    L0727

                lbra    L04EB

L068C           ldd     <20,Y
                lbsr    L0D9E

                ldd     2,Y
                beq     L06B0

                ldd     <16,Y
                subd    #$0001
                std     <16,Y
                ldd     <16,Y
                subd    #$0001
                std     <16,Y
                leax    265,Y
                leax    D,X
                clr     ,X
L06B0           ldd     14,Y
                subd    #$0001
                std     14,Y
                leas    254,S
                rts
L06BC           pshs    B,A
                clr     153,Y
L06C2           ldb     153,Y
                bne     L06EF

                ldd     <34,Y
                addd    #$0020
                std     <34,Y
                ldd     #$0020
                pshs    B,A
                leax    153,Y
                pshs    X
                ldd     4,S
                lbsr    L0DA1

                leas    4,S
                cmpd    #$0020
                beq     L06C2

                ldd     #$FFFF
                leas    2,S
                rts
L06EF           ldd     #$0001
                leas    2,S
                rts
L06F5           pshs    B,A
                clrb
                clra
                pshs    B,A
                ldd     <34,Y
                pshs    B,A
                ldd     <32,Y
                pshs    B,A
                ldd     6,S
                lbsr    L0DA4

                leas    6,S
                cmpd    #$0000
                bge     L0724

                leax    L0BCF,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                ldd     <u0002
                lbsr    L0D98

L0724           leas    2,S
                rts
L0727           pshs    B,A
                ldd     ,S
                lbsr    L07F5

                ldd     14,Y
                cmpd    #$0001
                lbne    L07A6

                leax    L0BE2,PCR
                pshs    X
                ldd     2,S
                lbsr    L08AC

                leas    2,S
                cmpd    #$0000
                lbne    L07A6

                ldd     ,Y
                cmpd    #$0001
                bne     L0777

                leax    335,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    <81,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0BEA,PCR
                tfr     X,D
                lbsr    L0D92

                leas    8,S
L0777           ldd     ,Y
                cmpd    #$0002
                bne     L07A1

                leax    185,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    <81,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0C05,PCR
                tfr     X,D
                lbsr    L0D92

                leas    8,S
L07A1           clrb
                clra
                leas    2,S
                rts
L07A6           ldd     ,S
                pshs    B,A
                ldd     2,S
                pshs    B,A
                leax    335,Y
                pshs    X
                leax    <36,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0C18,PCR
                tfr     X,D
                lbsr    L0D92

                leas    10,S
                ldd     12,Y
                beq     L07F2

                ldd     ,S
                pshs    B,A
                ldd     2,S
                pshs    B,A
                leax    335,Y
                pshs    X
                leax    <36,Y
                pshs    X
                leax    265,Y
                pshs    X
                leax    L0C2C,PCR
                tfr     X,D
                lbsr    L0D92

                leas    10,S
L07F2           leas    2,S
                rts
L07F5           pshs    B,A
                leas    -4,S
                clrb
                clra
                std     2,S
                std     ,S
L07FF           ldd     4,S
                ldx     ,S
                ldb     D,X
                andb    #$80
                cmpb    #$00
                bne     L081C

                ldd     ,S
                cmpd    #$001D
                bgt     L081C

                ldd     ,S
                addd    #$0001
                std     ,S
                bra     L07FF

L081C           ldd     4,S
                ldx     ,S
                leax    D,X
                pshs    X
                ldd     6,S
                ldx     2,S
                ldb     D,X
                andb    #$7F
                stb     [,S++]
                ldd     ,S
                addd    #$0001
                std     ,S
                ldd     4,S
                ldx     ,S
                leax    D,X
                clr     ,X
                leas    6,S
                rts
L0840           ldd     #$0081
                pshs    B,A
                leax    L0C41,PCR
                tfr     X,D
                lbsr    L0D9B

                leas    2,S
                std     <22,Y
                ldd     #$0020
                pshs    B,A
                leax    153,Y
                pshs    X
                ldd     <22,Y
                lbsr    L0DA1

                leas    4,S
                ldd     #$0020
                pshs    B,A
                leax    <121,Y
                pshs    X
                ldd     <22,Y
                lbsr    L0DA1

                leas    4,S
                leax    153,Y
                ldb     <31,X
                leax    <121,Y
                cmpb    <31,X
                bne     L08A9

                leax    153,Y
                ldb     <30,X
                leax    <121,Y
                cmpb    <30,X
                bne     L08A9

                leax    153,Y
                ldb     <29,X
                leax    <121,Y
                cmpb    <29,X
                bne     L08A9

                ldd     #$0001
                rts
L08A9           clrb
                clra
                rts
L08AC           pshs    B,A
                leas    -3,S
                clrb
                clra
                std     1,S
L08B4           ldd     3,S
                ldx     1,S
                ldb     D,X
                beq     L08E1

                ldd     3,S
                ldx     1,S
                ldb     D,X
                orb     #$20
                stb     ,S
                tstb
                beq     L08E1

                ldd     1,S
                addd    #$0001
                std     1,S
                subd    #$0001
                ldx     7,S
                ldb     D,X
                cmpb    ,S
                beq     L08B4

                ldd     #$0001
                leas    5,S
                rts
L08E1           ldd     7,S
                ldx     1,S
                ldb     D,X
                clra
                leas    5,S
                rts
L08EB           pshs    B,A
                leas    -4,S
                clrb
                clra
                std     ,S
L08F3           ldd     4,S
                ldx     ,S
                ldb     D,X
                beq     L0904

                ldd     ,S
                addd    #$0001
                std     ,S
                bra     L08F3

L0904           clrb
                clra
                std     2,S
L0908           ldd     ,S
                addd    #$0001
                std     ,S
                subd    #$0001
                ldx     4,S
                leax    D,X
                pshs    X
                ldd     10,S
                ldx     4,S
                ldb     D,X
                stb     [,S++]
                ldd     2,S
                addd    #$0001
                std     2,S
                subd    #$0001
                ldx     8,S
                ldb     D,X
                cmpb    #$00
                beq     L093A

                ldd     ,S
                cmpd    #$00F9
                blt     L0908

L093A           ldd     ,S
                cmpd    #$00F9
                blt     L0954

                leax    L0C43,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                clrb
                clra
                lbsr    L0D98

L0954           leas    6,S
                rts
L0957           pshs    B,A
                leas    -2,S
                clrb
                clra
                std     ,S
L095F           ldd     2,S
                ldx     ,S
                ldb     D,X
                beq     L0970

                ldd     ,S
                addd    #$0001
                std     ,S
                bra     L095F

L0970           ldd     ,S
                cmpd    #$0000
                ble     L09A4

                ldd     <18,Y
                cmpd    #$0000
                blt     L09A4

                ldd     <18,Y
                subd    #$0001
                std     <18,Y
                addd    #$0001
                leax    335,Y
                leax    D,X
                pshs    X
                ldd     2,S
                subd    #$0001
                std     2,S
                ldx     4,S
                ldb     D,X
                stb     [,S++]
                bra     L0970

L09A4           ldd     <18,Y
                cmpd    #$0000
                bgt     L09BF

                leax    L0C5F,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                clrb
                clra
                lbsr    L0D98

L09BF           leas    4,S
                rts
L09C2           pshs    B,A
                ldd     ,S
                lbsr    L0DA7

                cmpd    #$0000
                bge     L09E5

                ldd     ,S
                pshs    B,A
                leax    L0C7B,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    4,S
                ldd     <u0002
                lbsr    L0D98

L09E5           leas    2,S
                rts
L09E8           leax    L0CA1,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0CC9,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0CEA,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0D0C,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0D2F,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0D4C,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                leax    L0D6B,PCR
                pshs    X
                ldd     <u000E
                lbsr    L0D8F

                leas    2,S
                clrb
                clra
                lbra    L0D98

L0A48           FCC     "* Skipping directory: '%s'.  Error #%d"

                FCB     $0A,$00

L0A70           FCC     "dsave: unknown option- %c"

                FCB     $0A,$0A,$00

L0A8C           FCB     $2F,$00

L0A8E           FCC     "/D0"

                FCB     $00

L0A92           FCB     $2F,$00

L0A94           FCC     "/D1"

                FCB     $00

L0A98           FCC     "dsave: unprocessed arguments"

                FCB     $0A,$0A,$00

L0AB7           FCB     $74,$0A,$00

L0ABA           FCC     "chd %s"

                FCB     $0A,$00

L0AC2           FCC     "tmode .1 -pause"

                FCB     $0A,$00

L0AD3           FCC     "load copy"

                FCB     $0A,$00

L0ADE           FCC     "load same"

                FCB     $0A,$00

L0AE9           FCB     $2E,$2E,$00

L0AEC           FCB     $72,$00

L0AEE           FCC     "dsave: error in reading path"

                FCB     $0A,$00

L0B0C           FCB     $2F,$00

L0B0E           FCB     $2E,$2E,$00

L0B11           FCB     $72,$00

L0B13           FCB     $72,$00

L0B15           FCC     "unlink same"

                FCB     $0A,$00

L0B22           FCC     "unlink copy"

                FCB     $0A,$00

L0B2F           FCC     "tmode .1 pause"

                FCB     $0A,$00

L0B3F           FCC     "dsave: dir level too deep for indent"

                FCB     $0A,$00

L0B65           FCB     $2E,$00

L0B67           FCC     "dsave: cannot open directory"

                FCB     $0A,$00

L0B85           FCC     "%sMakdir %s"

                FCB     $0A,$00

L0B92           FCC     "%sChd %s"

                FCB     $0A,$00

L0B9C           FCB     $72,$00

L0B9E           FCB     $2F,$00

L0BA0           FCC     "%sChd .."

                FCB     $0A,$00

L0BAA           FCB     $2E,$2E,$00

L0BAD           FCB     $72,$00

L0BAF           FCB     $2E,$00

L0BB1           FCC     "dsave: cannot open directory"

                FCB     $0A,$00

L0BCF           FCC     "dsave: seek error"

                FCB     $0A,$00

L0BE2           FCC     "os9boot"

                FCB     $00

L0BEA           FCC     "%sOS9Gen %s"

                FCB     $0A

                FCC     "%s%s/OS9Boot"

                FCB     $0A,$0A,$00

L0C05           FCC     "%sOS9Gen %s"

                FCB     $0A

                FCC     "%s%s"

                FCB     $0A,$0A,$00

L0C18           FCC     "%sCopy %s %s/%s %s"

                FCB     $0A,$00

L0C2C           FCC     "%sSame  %s %s/%s %s"

                FCB     $0A,$00

L0C41           FCB     $2E,$00

L0C43           FCC     "dsave: error path too long"

                FCB     $0A,$00

L0C5F           FCC     "dsave: error path too long"

                FCB     $0A,$00

L0C7B           FCC     "dsave: cannot change directory to %s"

                FCB     $0A,$00

L0CA1           FCC     "Use: DSAVE <-opts> <from dev> <to dev>"

                FCB     $0A,$00

L0CC9           FCC     "     opts  b = include bootfile"

                FCB     $0A,$00

L0CEA           FCC     "           i = indent dir levels"

                FCB     $0A,$00

L0D0C           FCC     "           l = only one dir level"

                FCB     $0A,$00

L0D2F           FCC     "           m = omit makdirs"

                FCB     $0A,$00

L0D4C           FCC     "           s = alter mem size"

                FCB     $0A,$00

L0D6B           FCC     "           v = verify copied files"

                FCB     $0A,$00

L0D8F           lbra    L0E55

L0D92           lbra    L0E8A

L0D95           lbra    L1189

L0D98           lbra    L0E3A

L0D9B           lbra    L18BD

L0D9E           lbra    L18CA

L0DA1           lbra    L18D4

L0DA4           lbra    L196D

L0DA7           lbra    L18AB

start           clrb			; clear first byte of storage (ParamCount)	
                stb     ,U++
		
L0DAD           lda     ,X+		; get a byte of parameters
                cmpa    #C$CR		; CR ?
                beq     L0DCB		; yes skip

                bsr     CkSpace9	; space?

                beq     L0DAD		; yes loop and process next

                leax    -1,X		; step pointer back one
                stx     ,U++		; save pointer to first param
                inc     <ParamCount	; increment param count

; look for next param
L0DBD           lda     ,X+		; get a byte of parameters
                cmpa    #C$CR		; CR ?
                beq     L0DCB		; yes skip

                bsr     CkSpace9	; space?

                bne     L0DBD		; no, loop again

                clr     -1,X		; replace with zero
                bra     L0DAD		

L0DCB           clr     ,-X		; replace it with a zero
                lda     <ParamCount
                sta     <u0001
                clra			; d=0
                clrb
                pshs    B,A
		
L0DD5           tst     <ParamCount	; any params?
                beq     L0DE1		; no skip

                dec     <ParamCount	; decrement paramcount
                ldd     ,--U		; get pointer to command
                pshs    B,A		; save it on stack
                bra     L0DD5		; keep going untill all params done

L0DE1           pshs    X		; save x
                leax    ,S		; x=s
                pshs    X		; push it
                leax    -2,S		; back 2
                pshs    X		; push it
                leax    ,U		; point x at perminent store
		
L0DED           clr     ,X+
                cmpx    ,S
                blo     L0DED

                puls    X
                leau    -2,U
                leax    <u0010,U
                stx     <u000A
                stx     <u0008
                leax    <u001B,U
                stx     <u000C
                stx     <u0018
                leax    <u0026,U
                stx     <u000E
                stx     <u0023
                lda     #$05
                sta     <u0016
                lda     #$06
                sta     <u0021
                lda     #$06
                sta     <u002C
                clra
                sta     <u0017
                inca
                sta     <u0022
                inca
                sta     <u002D
                ldd     ,U
                addd    #$0001
                sty     <ParamCount
                leay    u00FD,U
                lbsr    L000D

                lbra    L0E3A

CkSpace9        cmpa    #' '		; space?		
                beq     L0E39		; yes, return

                cmpa    #$09		; CTRL-I ?
L0E39           rts			

L0E3A           pshs    B,A
L0E3C           ldd     <u0008
                beq     L0E47

                ldd     <u0008
                lbsr    L0E4F

                bra     L0E3C

L0E47           ldd     ,S
                lbsr    L0E52

                leas    2,S
                rts
L0E4F           lbra    L1189

L0E52           lbra    L19AB

L0E55           pshs    B,A
                leas    -256,S
                leax    262,S
                pshs    X
                ldd     262,S
                pshs    B,A
                leax    4,S
                tfr     X,D
                lbsr    L0E84

                leas    4,S
                ldd     256,S
                pshs    B,A
                leax    2,S
                tfr     X,D
                lbsr    L0E87

                leas    2,S
                leas    258,S
                rts
L0E84           lbra    L0EB7

L0E87           lbra    L12E2

L0E8A           pshs    B,A
                leas    -256,S
                leax    260,S
                pshs    X
                ldd     258,S
                pshs    B,A
                leax    4,S
                tfr     X,D
                lbsr    L0EB1

                leas    4,S
                leax    ,S
                tfr     X,D
                lbsr    L0EB4

                leas    258,S
                rts
L0EB1           lbra    L0EB7

L0EB4           lbra    L127D

L0EB7           pshs    B,A
                leas    -269,S
L0EBD           ldx     273,S
                ldb     ,X+
                stx     273,S
                stb     ,S
                tstb
                lbeq    L10EA

                ldb     ,S
                cmpb    #$25
                beq     L0EEA

                ldd     269,S
                addd    #$0001
                std     269,S
                subd    #$0001
                pshs    B,A
                ldb     2,S
                stb     [,S++]
                bra     L0EBD

L0EEA           leax    13,S
                stx     9,S
                ldd     #$0006
                std     4,S
                clr     7,S
                ldb     #$20
                stb     8,S
                clr     6,S
                ldb     [273,S]
                cmpb    #$2D
                bne     L0F12

                ldd     273,S
                addd    #$0001
                std     273,S
                ldb     #$01
                stb     7,S
L0F12           ldb     [273,S]
                clra
                lbsr    L1177

                addd    #$0000
                beq     L0F38

                ldb     [273,S]
                cmpb    #$30
                bne     L0F2B

                ldb     #$30
                stb     8,S
L0F2B           leax    273,S
                tfr     X,D
                lbsr    L117A

                std     2,S
                bra     L0F3C

L0F38           clrb
                clra
                std     2,S
L0F3C           ldb     [273,S]
                cmpb    #$2E
                bne     L0F5E

                ldd     273,S
                addd    #$0001
                std     273,S
                leax    273,S
                tfr     X,D
                lbsr    L117A

                std     4,S
                ldb     #$01
                stb     6,S
L0F5E           ldx     273,S
                ldb     ,X+
                stx     273,S
                stb     ,S
                ldb     ,S
                clra
                lbsr    L117D

                cmpb    #$64
                beq     L0F93

                cmpb    #$75
                beq     L0FC0

                cmpb    #$78
                lbeq    L0FC6

                cmpb    #$6F
                lbeq    L0FCC

                cmpb    #$63
                lbeq    L0FF6

                cmpb    #$73
                lbeq    L1018

                lbra    L10D3

L0F93           ldd     [275,S]
                cmpd    #$0000
                bge     L0FC0

                ldd     9,S
                addd    #$0001
                std     9,S
                subd    #$0001
                pshs    B,A
                ldb     #$2D
                stb     [,S++]
                ldd     [275,S]
                nega
                negb
                sbca    #$00
                std     [275,S]
                ldd     2,S
                subd    #$0001
                std     2,S
L0FC0           ldb     #$0A
                stb     1,S
                bra     L0FD0

L0FC6           ldb     #$10
                stb     1,S
                bra     L0FD0

L0FCC           ldb     #$08
                stb     1,S
L0FD0           ldb     1,S
                clra
                pshs    B,A
                ldx     277,S
                ldd     ,X++
                stx     277,S
                pshs    B,A
                leax    13,S
                tfr     X,D
                lbsr    L10F3

                leas    4,S
                clra
                pshs    B,A
                ldd     4,S
                subd    ,S++
                std     2,S
                lbra    L105A

L0FF6           ldd     9,S
                addd    #$0001
                std     9,S
                subd    #$0001
                pshs    B,A
                ldx     277,S
                ldd     ,X++
                stx     277,S
                stb     [,S++]
                ldd     2,S
                subd    #$0001
                std     2,S
                lbra    L105A

L1018           ldb     6,S
                bne     L1021

                ldd     #$0100
                std     4,S
L1021           ldx     275,S
                ldd     ,X++
                stx     275,S
                std     11,S
L102D           ldb     [<11,S]
                beq     L105A

                ldd     4,S
                beq     L105A

                ldd     9,S
                addd    #$0001
                std     9,S
                subd    #$0001
                pshs    B,A
                ldx     13,S
                ldb     ,X+
                stx     13,S
                stb     [,S++]
                ldd     4,S
                subd    #$0001
                std     4,S
                ldd     2,S
                subd    #$0001
                std     2,S
                bra     L102D

L105A           clr     [<9,S]
                leax    13,S
                stx     9,S
                ldb     7,S
                bne     L108B

L1065           ldd     2,S
                subd    #$0001
                std     2,S
                addd    #$0001
                cmpd    #$0000
                ble     L108B

                ldd     269,S
                addd    #$0001
                std     269,S
                subd    #$0001
                pshs    B,A
                ldb     10,S
                stb     [,S++]
                bra     L1065

L108B           ldx     9,S
                ldb     ,X+
                stx     9,S
                stb     [269,S]
                tstb
                beq     L10A5

                ldd     269,S
                addd    #$0001
                std     269,S
                bra     L108B

L10A5           ldb     7,S
                lbeq    L0EBD

L10AB           ldd     2,S
                subd    #$0001
                std     2,S
                addd    #$0001
                cmpd    #$0000
                lble    L0EBD

                ldd     269,S
                addd    #$0001
                std     269,S
                subd    #$0001
                pshs    B,A
                ldb     10,S
                stb     [,S++]
                bra     L10AB

L10D3           ldd     269,S
                addd    #$0001
                std     269,S
                subd    #$0001
                pshs    B,A
                ldb     2,S
                stb     [,S++]
                lbra    L0EBD

L10EA           clr     [269,S]
                leas    271,S
                rts
L10F3           pshs    B,A
                leas    -3,S
                ldd     [<3,S]
                std     ,S
                ldd     7,S
                pshs    B,A
                ldd     11,S
                lbsr    L1183

                stb     2,S
                ldd     [<3,S]
                addd    #$0001
                std     [<3,S]
                subd    #$0001
                pshs    B,A
                ldb     4,S
                cmpb    #$0A
                bhs     L1121

                ldb     4,S
                addb    #$30
                bra     L1125

L1121           ldb     4,S
                addb    #$37
L1125           stb     [,S++]
L1127           ldd     7,S
                pshs    B,A
                ldd     11,S
                lbsr    L1186

                std     7,S
                addd    #$0000
                beq     L1164

                ldd     7,S
                pshs    B,A
                ldd     11,S
                lbsr    L1183

                stb     2,S
                ldd     [<3,S]
                addd    #$0001
                std     [<3,S]
                subd    #$0001
                pshs    B,A
                ldb     4,S
                cmpb    #$0A
                bhs     L115C

                ldb     4,S
                addb    #$30
                bra     L1160

L115C           ldb     4,S
                addb    #$37
L1160           stb     [,S++]
                bra     L1127

L1164           ldx     [<3,S]
                clr     ,X
                ldd     ,S
                lbsr    L1180

                ldd     [<3,S]
                subd    ,S
                clra
                leas    5,S
                rts
L1177           lbra    L173A

L117A           lbra    L123E

L117D           lbra    L1751

L1180           lbra    L1290

L1183           lbra    L17EF

L1186           lbra    L180B

L1189           pshs    B,A
                leas    -5,S
                clrb
                clra
                std     2,S
                ldd     <u0008
                std     ,S
L1195           ldd     ,S
                lbeq    L11F8

                ldd     ,S
                cmpd    5,S
                lbne    L11EB

                ldd     2,S
                beq     L11B7

                ldd     2,S
                addd    #$0008
                pshs    B,A
                ldx     2,S
                ldd     8,X
                std     [,S++]
                bra     L11BD

L11B7           ldx     ,S
                ldd     8,X
                std     <u0008
L11BD           clr     4,S
                ldd     5,S
                lbsr    L1235

                ldx     5,S
                ldb     7,X
                clra
                lbsr    L1238

                cmpd    #$FFFF
                bne     L11D6

                ldb     #$01
                stb     4,S
L11D6           ldd     5,S
                lbsr    L11FE

                ldb     4,S
                beq     L11E5

                ldd     #$FFFF
                leas    7,S
                rts
L11E5           ldd     #$0001
                leas    7,S
                rts
L11EB           ldd     ,S
                std     2,S
                ldx     ,S
                ldd     8,X
                std     ,S
                lbra    L1195

L11F8           ldd     #$FFFF
                leas    7,S
                rts
L11FE           pshs    B,A
                ldd     ,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$08
                tstb
                beq     L1215

                ldx     ,S
                ldd     4,X
                lbsr    L123B

L1215           ldd     ,S
                cmpd    <u000A
                beq     L122A

                ldd     ,S
                cmpd    <u000C
                beq     L122A

                ldd     ,S
                cmpd    <u000E
                bne     L122D

L122A           leas    2,S
                rts
L122D           ldd     ,S
                lbsr    L123B

                leas    2,S
                rts
L1235           lbra    L1597

L1238           lbra    L18CA

L123B           lbra    L1683

L123E           pshs    B,A
                leas    -2,S
                clrb
                clra
                std     ,S
L1246           ldx     [<2,S]
                ldb     ,X
                clra
                lbsr    L1277

                addd    #$0000
                beq     L1272

                ldx     [<2,S]
                ldb     ,X+
                stx     [<2,S]
                clra
                pshs    B,A
                ldd     2,S
                pshs    B,A
                ldd     #$000A
                lbsr    L127A

                addd    ,S++
                subd    #$0030
                std     ,S
                bra     L1246

L1272           ldd     ,S
                leas    4,S
                rts
L1277           lbra    L173A

L127A           lbra    L17A0

L127D           pshs    B,A
                ldd     <u000C
                pshs    B,A
                ldd     2,S
                lbsr    L128D

                leas    2,S
                leas    2,S
                rts
L128D           lbra    L12E2

L1290           pshs    B,A
                leas    -5,S
                clrb
                clra
                std     ,S
                ldd     5,S
                lbsr    L12DF

                subd    #$0001
                std     2,S
L12A2           ldd     ,S
                cmpd    2,S
                lbge    L12DC

                ldd     ,S
                ldx     5,S
                ldb     D,X
                stb     4,S
                ldd     5,S
                addd    ,S
                pshs    B,A
                ldd     4,S
                ldx     7,S
                ldb     D,X
                stb     [,S++]
                ldd     5,S
                addd    2,S
                pshs    B,A
                ldb     6,S
                stb     [,S++]
                ldd     ,S
                addd    #$0001
                std     ,S
                ldd     2,S
                subd    #$0001
                std     2,S
                lbra    L12A2

L12DC           leas    7,S
                rts
L12DF           lbra    L1784

L12E2           pshs    B,A
                leas    -263,S
                ldd     267,S
                lbsr    L13D0

                ldd     263,S
                std     1,S
                leax    7,S
                stx     3,S
L12F9           ldb     [<1,S]
                lbeq    L1390

                ldb     [<1,S]
                cmpb    #$0A
                lbne    L1338

                ldb     #$0D
                stb     [<3,S]
                ldd     #$0100
                pshs    B,A
                leax    9,S
                pshs    X
                ldx     271,S
                ldb     7,X
                clra
                lbsr    L13D3

                leas    4,S
                cmpd    #$FFFF
                bne     L1331

                ldd     #$FFFF
                leas    265,S
                rts
L1331           leax    7,S
                stx     3,S
                lbra    L1386

L1338           ldb     [<1,S]
                cmpb    #$09
                bne     L1375

                ldd     3,S
                leax    7,S
                pshs    X
                subd    ,S++
                pshs    B,A
                ldd     #$0008
                lbsr    L13DC

                pshs    B,A
                ldd     #$0008
                subd    ,S++
                std     5,S
L1358           ldd     5,S
                beq     L1386

                ldd     3,S
                addd    #$0001
                std     3,S
                subd    #$0001
                pshs    B,A
                ldb     #$20
                stb     [,S++]
                ldd     5,S
                subd    #$0001
                std     5,S
                bra     L1358

L1375           ldd     3,S
                addd    #$0001
                std     3,S
                subd    #$0001
                pshs    B,A
                ldb     [<3,S]
                stb     [,S++]
L1386           ldd     1,S
                addd    #$0001
                std     1,S
                lbra    L12F9

L1390           clr     [<3,S]
                ldd     3,S
                leax    7,S
                pshs    X
                cmpd    ,S++
                lbeq    L13C7

                leax    7,S
                tfr     X,D
                lbsr    L13D9

                pshs    B,A
                leax    9,S
                pshs    X
                ldx     271,S
                ldb     7,X
                clra
                lbsr    L13D6

                leas    4,S
                cmpd    #$FFFF
                bne     L13C7

                ldd     #$FFFF
                leas    265,S
                rts
L13C7           ldd     263,S
                leas    265,S
                rts
L13D0           lbra    L1597

L13D3           lbra    L1913

L13D6           lbra    L18E9

L13D9           lbra    L1784

L13DC           lbra    L17E3

                pshs    B,A
                ldd     <u000C
                pshs    B,A
                ldd     2,S
                lbsr    L13EF

                leas    2,S
                leas    2,S
                rts
L13EF           pshs    B,A
                ldd     4,S
                addd    #$0002
                tfr     D,X
                ldd     ,X
                subd    #$0001
                std     ,X
                cmpd    #$0000
                blt     L141A

                ldd     [<4,S]
                addd    #$0001
                std     [<4,S]
                subd    #$0001
                pshs    B,A
                ldb     3,S
                stb     [,S++]
                clra
                bra     L142B

L141A           ldd     #$0001
                pshs    B,A
                ldd     6,S
                pshs    B,A
                ldb     5,S
                clra
                lbsr    L143E

                leas    4,S
L142B           leas    2,S
                rts
                pshs    B,A
                ldd     <u000E
                pshs    B,A
                ldd     2,S
                lbsr    L13EF

                leas    2,S
                leas    2,S
                rts
L143E           pshs    B,A
                leas    -3,S
                ldb     4,S
                stb     2,S
                ldd     7,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$02
                cmpb    #$00
                beq     L1464

                ldd     7,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$30
                cmpb    #$00
                beq     L146A

L1464           ldd     #$FFFF
                leas    5,S
                rts
L146A           ldb     10,S
                beq     L1473

                ldd     #$0001
                bra     L1475

L1473           clrb
                clra
L1475           pshs    B,A
                ldd     9,S
                addd    #$0002
                pshs    B,A
                ldd     #$0100
                subd    [,S++]
                subd    ,S++
                std     ,S
                ldd     7,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$04
                cmpb    #$00
                lbne    L14C6

                ldx     7,S
                ldd     4,X
                bne     L14C6

                ldd     7,S
                addd    #$0004
                pshs    B,A
                ldd     #$0100
                lbsr    L15AB

                std     [,S++]
                cmpd    #$0000
                bne     L14C2

                ldd     7,S
                addd    #$0006
                tfr     D,U
                ldb     ,U
                orb     #$04
                stb     ,U
                bra     L14C6

L14C2           clrb
                clra
                std     ,S
L14C6           ldd     7,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$04
                tstb
                lbeq    L150A

                ldb     10,S
                lbeq    L153B

                ldd     #$0001
                pshs    B,A
                leax    4,S
                pshs    X
                ldx     11,S
                ldb     7,X
                clra
                lbsr    L15AE

                leas    4,S
                cmpd    #$FFFF
                lbne    L153B

                ldd     7,S
                addd    #$0006
                tfr     D,U
                ldb     ,U
                orb     #$20
                stb     ,U
                ldd     #$FFFF
                leas    5,S
                rts
L150A           ldd     ,S
                beq     L153B

                ldd     ,S
                pshs    B,A
                ldx     9,S
                ldd     4,X
                pshs    B,A
                ldx     11,S
                ldb     7,X
                clra
                lbsr    L15AE

                leas    4,S
                cmpd    #$FFFF
                bne     L153B

                ldd     7,S
                addd    #$0006
                tfr     D,U
                ldb     ,U
                orb     #$20
                stb     ,U
                ldd     #$FFFF
                leas    5,S
                rts
L153B           ldd     7,S
                addd    #$0002
                pshs    B,A
                ldd     9,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$04
                tstb
                beq     L1554

                clrb
                clra
                bra     L1557

L1554           ldd     #$0100
L1557           std     [,S++]
                ldx     7,S
                ldd     4,X
                std     [<7,S]
                ldb     10,S
                beq     L1591

                ldd     7,S
                addd    #$0006
                tfr     D,X
                ldb     ,X
                andb    #$04
                cmpb    #$00
                bne     L1591

                ldd     [<7,S]
                addd    #$0001
                std     [<7,S]
                subd    #$0001
                pshs    B,A
                ldb     6,S
                stb     [,S++]
                ldd     7,S
                addd    #$0002
                pshs    B,A
                ldd     #$00FF
                std     [,S++]
L1591           ldb     4,S
                clra
                leas    5,S
                rts
L1597           pshs    B,A
                clrb
                clra
                pshs    B,A
                ldd     2,S
                pshs    B,A
                clrb
                clra
                lbsr    L143E

                leas    4,S
                leas    2,S
                rts
L15AB           lbra    L15B1

L15AE           lbra    L18E9

L15B1           pshs    B,A
                leas    -8,S
                ldd     8,S
                addd    #$0003
                lsra
                rorb
                lsra
                rorb
                addd    #$0001
                std     6,S
                ldd     591,Y
                std     2,S
                cmpd    #$0000
                bne     L15E3

                leax    587,Y
                stx     2,S
                stx     591,Y
                stx     587,Y
                clrb
                clra
                std     589,Y
L15E3           ldd     [<2,S]
                std     ,S
L15E8           ldx     ,S
                ldd     2,X
                cmpd    6,S
                lblo    L163E

                ldx     ,S
                ldd     2,X
                cmpd    6,S
                bne     L1603

                ldd     [,S]
                std     [<2,S]
                bra     L1630

L1603           ldd     ,S
                addd    #$0002
                tfr     D,U
                ldd     ,U
                subd    6,S
                std     ,U
                ldd     ,S
                addd    #$0002
                tfr     D,X
                ldd     ,X
                lslb
                rola
                lslb
                rola
                pshs    B,A
                ldd     2,S
                addd    ,S++
                std     ,S
                ldd     ,S
                addd    #$0002
                pshs    B,A
                ldd     8,S
                std     [,S++]
L1630           ldd     2,S
                std     591,Y
                ldd     ,S
                addd    #$0004
                leas    10,S
                rts
L163E           ldd     ,S
                cmpd    591,Y
                lbne    L1678

                ldd     6,S
                lslb
                rola
                lslb
                rola
                lbsr    L1737

                std     4,S
                cmpd    #$FFFF
                bne     L165F

                clrb
                clra
                leas    10,S
                rts
L165F           ldd     4,S
                addd    #$0002
                pshs    B,A
                ldd     8,S
                std     [,S++]
                ldd     4,S
                addd    #$0004
                lbsr    L1683

                ldd     591,Y
                std     ,S
L1678           ldd     ,S
                std     2,S
                ldd     [,S]
                std     ,S
                lbra    L15E8

L1683           pshs    B,A
                leas    -4,S
                ldd     4,S
                subd    #$0004
                std     ,S
                ldd     591,Y
                std     2,S
L1694           ldd     ,S
                cmpd    2,S
                bls     L16A3

                ldd     ,S
                cmpd    [<2,S]
                blo     L16C2

L16A3           ldd     2,S
                cmpd    [<2,S]
                blo     L16BA

                ldd     ,S
                cmpd    2,S
                bhi     L16C2

                ldd     ,S
                cmpd    [<2,S]
                blo     L16C2

L16BA           ldd     [<2,S]
                std     2,S
                lbra    L1694

L16C2           ldd     ,S
                addd    #$0002
                tfr     D,X
                ldd     ,X
                lslb
                rola
                lslb
                rola
                addd    ,S
                cmpd    [<2,S]
                bne     L16F5

                ldd     [<2,S]
                addd    #$0002
                pshs    B,A
                ldd     2,S
                addd    #$0002
                tfr     D,U
                ldd     ,U
                addd    [,S++]
                std     ,U
                ldx     [<2,S]
                ldd     ,X
                std     [,S]
                bra     L16FA

L16F5           ldd     [<2,S]
                std     [,S]
L16FA           ldd     2,S
                addd    #$0002
                tfr     D,X
                ldd     ,X
                lslb
                rola
                lslb
                rola
                addd    2,S
                cmpd    ,S
                bne     L1729

                ldd     ,S
                addd    #$0002
                pshs    B,A
                ldd     4,S
                addd    #$0002
                tfr     D,U
                ldd     ,U
                addd    [,S++]
                std     ,U
                ldd     [,S]
                std     [<2,S]
                bra     L172E

L1729           ldd     ,S
                std     [<2,S]
L172E           ldd     2,S
                std     591,Y
                leas    6,S
                rts
L1737           lbra    L1882

L173A           pshs    B,A
                ldb     1,S
                cmpb    #$30
                blo     L174C

                ldb     1,S
                cmpb    #$39
                bhi     L174C

                ldb     #$01
                bra     L174D

L174C           clrb
L174D           clra
                leas    2,S
                rts
L1751           pshs    B,A
                ldb     1,S
                clra
                lbsr    L176A

                addd    #$0000
                beq     L1764

                ldb     1,S
                addb    #$20
                bra     L1766

L1764           ldb     1,S
L1766           clra
                leas    2,S
                rts
L176A           lbra    L176D

L176D           pshs    B,A
                ldb     1,S
                cmpb    #$41
                blo     L177F

                ldb     1,S
                cmpb    #$5A
                bhi     L177F

                ldb     #$01
                bra     L1780

L177F           clrb
L1780           clra
                leas    2,S
                rts
L1784           pshs    B,A
                leas    -2,S
                ldd     2,S
                std     ,S
L178C           ldb     [,S]
                beq     L1799

                ldd     ,S
                addd    #$0001
                std     ,S
                bra     L178C

L1799           ldd     ,S
                subd    2,S
                leas    4,S
                rts
L17A0           leas    -5,S
                clr     ,S
                bsr     L17D9

                std     1,S
                ldd     7,S
                bsr     L17D9

                std     7,S
                lda     2,S
                ldb     8,S
                mul
                std     3,S
                lda     1,S
                ldb     8,S
                mul
                tfr     B,A
                clrb
                addd    3,S
                std     3,S
                lda     2,S
                ldb     7,S
                mul
                tfr     B,A
                clrb
                addd    3,S
                tst     ,S
                bpl     L17D3

                nega
                negb
                sbca    #$00
L17D3           ldx     5,S
                leas    9,S
                jmp     ,X

L17D9           tsta
                bpl     L17E2

                com     2,S
                nega
                negb
                sbca    #$00
L17E2           rts
L17E3           ldx     2,S
                bsr     L1862

                pshs    CC
                stx     3,S
                puls    CC
                bra     L17F1

L17EF           andcc   #$F7
L17F1           orcc    #$01
                pshs    CC
                ldx     #$0000
                puls    CC
                bra     L1810

                ldx     2,S
                bsr     L1862

                pshs    CC
                stx     3,S
                ldx     #$7FFF
                puls    CC
                bra     L1810

L180B           ldx     #$FFFF
                andcc   #$F6
L1810           leas    -3,S
                pshs    CC
                std     2,S
                bne     L181E

                puls    CC
                tfr     X,D
                bra     L185C

L181E           lda     #$01
                sta     1,S
L1822           tst     2,S
                bmi     L182E

                lsl     3,S
                rol     2,S
                inc     1,S
                bra     L1822

L182E           ldd     6,S
                clr     6,S
                clr     7,S
L1834           subd    2,S
                bhs     L183E

                addd    2,S
                andcc   #$FE
                bra     L1840

L183E           orcc    #$01
L1840           rol     7,S
                rol     6,S
                lsr     2,S
                ror     3,S
                dec     1,S
                bne     L1834

                puls    CC
                blo     L1856

                pshs    CC
                ldd     6,S
                puls    CC
L1856           bpl     L185C

                nega
                negb
                sbca    #$00
L185C           ldx     3,S
                leas    7,S
                jmp     ,X

L1862           pshs    U
                tfr     D,U
                pshs    X
                eora    ,S++
                andcc   #$FE
                pshs    CC
                tfr     X,D
                bsr     L187A

                tfr     D,X
                tfr     U,D
                bsr     L187A

                puls    PC,U,CC         ; Pull of PC, effective RTS
L187A           tsta
                bpl     L1881

                nega
                negb
                sbca    #$00
L1881           rts
L1882           pshs    B,A
                leax    -253,Y
                tfr     X,D
                nega
                negb
                sbca    #$00
                addd    <ParamCount
                addd    ,S
                pshs    Y
                os9     F$Mem
                puls    Y
                puls    X
                blo     L18A4

                ldd     <ParamCount
                leax    D,X
                stx     <ParamCount
                rts
L18A4           clra
                std     <u0002
                ldd     #$FFFF
                rts
L18AB           tfr     D,X
                lda     #$01
                os9     I$ChgDir
                bhs     L18BA

                std     <u0002
                ldd     #$FFFF
                rts
L18BA           clra
                clrb
                rts
L18BD           tfr     D,X
                lda     3,S
                os9     I$Open
                blo     L1928

                tfr     A,B
                clra
                rts
L18CA           tfr     B,A
                os9     I$Close
                blo     L1928

                clra
                clrb
                rts
L18D4           pshs    Y
                tfr     B,A
                ldx     4,S
                ldy     6,S
                os9     I$Read
                puls    X
                exg     X,Y
                blo     L1928

                tfr     X,D
                rts
L18E9           pshs    Y
                tfr     B,A
                ldx     4,S
                ldy     6,S
                os9     I$Write
                puls    X
                exg     X,Y
                blo     L1928

                tfr     X,D
                rts
                pshs    Y
                tfr     B,A
                ldx     4,S
                ldy     6,S
                os9     I$ReadLn
                puls    X
                exg     X,Y
                blo     L1928

                tfr     X,D
                rts
L1913           pshs    Y
                tfr     B,A
                ldx     4,S
                ldy     6,S
                os9     I$WritLn
                puls    X
                exg     X,Y
                blo     L1928

                tfr     X,D
                rts
L1928           clra
                std     <u0002
                ldd     #$FFFF
                rts
                tfr     D,X
                lda     #$02
                ldb     3,S
                bmi     L193B

                tfr     B,A
                anda    #$03
L193B           orb     #$01
                pshs    X,B,A
                os9     I$Create
                puls    U,X
                exg     X,U
                bhs     L1958

                pshs    X
                os9     I$Delete
                puls    X
                blo     L1928

                tfr     U,D
                os9     I$Create
                blo     L1928

L1958           tfr     A,B
                clra
                rts
                pshs    B,A
                ldx     4,S
                ldd     ,X
                std     4,S
                clrb
                lda     2,X
                addd    6,S
                std     6,S
                puls    B,A
L196D           tfr     B,A
                ldb     7,S
                ldx     2,S
                ldu     4,S
                decb
                bne     L1981

                ldb     #$05
                os9     I$GetStt
                blo     L1928

                bra     L198B

L1981           decb
                bne     L1999

                ldb     #$02
                os9     I$GetStt
                blo     L1928

L198B           exg     D,U
                addd    4,S
                exg     D,U
                exg     D,X
                adcb    3,S
                adca    2,S
                exg     D,X
L1999           os9     I$Seek
                blo     L1928

                clra
                clrb
                rts
                tfr     D,X
                os9     I$Delete
                blo     L1928

                clra
                clrb
                rts
L19AB           os9     F$Exit
name            FCS     "dsave"         ; OS9 Module name

                FCB     $04

BeebDisEndAddr
;  "dsave_dis.bin",BeebDisStartAddr,BeebDisEndAddr

                emod
eom             equ     *
                end


