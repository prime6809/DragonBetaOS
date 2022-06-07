                nam     GRAPHICS        ; SCREEN DRIVER
                ttl     2D              ; VERSION
******21-05-84**********************************
*
*GRAPHICS SCREEN DRIVER PACKAGE - revised for new hardware
*
*TO HANDLE - 1. ESCAPE SEQUENCES - CREATED IN RESPONSE TO ADDITIONAL
*               COMMANDS IN GRAPHICS MODE
*CALLED BY SCFMAN WHICH PASSES ONE BYTE AT A
*TIME IN THE (a) REGISTER.
*
*WRITTEN BY S. HENDERSON OF VIVAWAY LIMITED AUG. 83-FEB. 84
*
*
                spc     2
                use     defsfile
                ttl     constant        ; definitions
                pag
*
*CONSTANTS
*
BCOUNT          equ     10              ; SIZE OF BUFFER
BLKMAP          equ     %00000111
BLKNUM          equ     %11111000
CHRARR          equ     20              ; TEXT CHARACTER ARRAY SIZE
CHRSIZ1         equ     10              ; TEXT CHARACTER HEIGHT
CHRSIZ2         equ     8               ; TEXT CHARACTER WIDTH
CIRCONS         equ     1144            ; CONSTANT USED IN CIRCLE PLOTTING
COLMAP          equ     $FCA0           ; BASE ADDRESS OF COLOUR MAPPER
CONT1           equ     $1F             ; )ASCII CONTROL CODES
CONT2           equ     $7F             ; )
COS0            equ     16384
CRTC            equ     $FC80           ; BASE ADDRESS OF CRT CONTROLLER
DIFF            equ     $8000           ; ADJUSTMENT FOR CLIPPING ROUTINE
EOSEQ           equ     $0D             ; ESCAPE SEQUENCE END CODE
ERROR1          equ     56              ; ERROR CODE 1 - PARAMETER ERROR
ERROR2          equ     50              ; ERROR CODE 2 - OVERFLOW ERROR
ERROR3          equ     57              ; ERROR CODE 3 - PAINT ROUTINE STACK EXCEEDS ALLOWED LIMIT
ESCODE          equ     $1B             ; ESCAPE SEQUENCE START CODE
FACT1           equ     160
FACT2           equ     78
FACT3           equ     158
FACT4           equ     313
FACT5           equ     633
FOURK           equ     $0FFF
HIRES           equ     4
HIXmax          equ     639
HIYmax          equ     511
LORES           equ     2
LOXmax          equ     319
LOYmax          equ     255
MAXCOL          equ     15              ; MAXIMUM COLOUR NUMBER
MAXITER         equ     16              ; MAX NO. OF ITERATIONS FOR CLIPPING ROUTINE
MAXPAG          equ     6
MAXSHFT         equ     7
MAXSTCK         equ     $D0             ; INITIAL PAINT ROUTINE STACK SIZE
MODE1           equ     1
MODE2           equ     2
MODE3           equ     3
MODE4           equ     4
MODLANG         equ     $41             ; DATA MODULE 6809 CODE
OFFSCRN         equ     999
PAGM2.3         equ     3
PAGM4           equ     1
PBLOCK          equ     181             ; SIZE OF VITAL PARAMETER BLOCK
PIA             equ     $FCC2           ; PORT B ADDRESS ON PIA
PIACLR1         equ     %11000011
PIASET1         equ     %00000000
PIASET2         equ     %00100000
PIASET3         equ     %00000100
PIASET4         equ     %00100100
PIASET5         equ     %00010000
REVS            equ     REENT+1
S4K             equ     $1000
SCAL0           equ     256
SHFTS           equ     $FF
SHMSK1          equ     %11111000
SHMSK2          equ     %11111100
SHMSK3          equ     %11110000
TWOK            equ     $07FF
TYPE            equ     SBRTN+OBJCT
                opt     -l
                ttl     Static          ; storage allocation
                opt     l
                pag
*
*VARIABLES
*
                use     gfxstat
GRMEM           equ     .
                spc     10
*
*MODULE HEADER
*
                mod     GREND,GRNAM,TYPE,REVS,PARTAB,GRMEM
EDITION         fcb     1
GRNAM           fcs     'gfxdrvr'
SINETAB         equ     *
                opt     -l
                use     sinetable
                ttl     parameter       ; acquisition and action routine tables
                opt     l
                pag
*
*TABLE OF PARAMETER ACQUISITION SUBROUTINES FOR GRAPHICS
*
                fdb     DISPG-TABLE1
                fdb     RETMEM-TABLE1   ; DEALLOCATE MEMORY VECTOR
PARTAB          fdb     $6172
TABLE1          fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     PLOTPAIR-TABLE1
                fdb     DRAWPAIR-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     GPARMS-TABLE1
                fdb     TEXTSTR-TABLE1
                spc     2
*
*TABLE OF ACTION SUBROUTINES
*
ACTAB           fdb     GMODE-ACTAB     ; a...$61
                fdb     GPAGE-ACTAB     ; b...$62
                fdb     CSET-ACTAB      ; c...$63
                fdb     GCLEAR-ACTAB    ; d...$64
                fdb     COLOUR-ACTAB    ; e...$65
                fdb     GRAPHICS-ACTAB  ; f...$66
                fdb     TRANS-ACTAB     ; g...$67
                fdb     SCALE-ACTAB     ; h...$68
                fdb     ROTATE-ACTAB    ; i...$69
                fdb     MOVE-ACTAB      ; j...$6A
                fdb     PLOT-ACTAB      ; k...$6B
                fdb     DRAW-ACTAB      ; l...$6C
                fdb     GGET-ACTAB      ; m...$6D
                fdb     GPUT-ACTAB      ; n...$6E
                fdb     PMODE-ACTAB     ; o...$6F
                fdb     PAINT-ACTAB     ; p...$70
                fdb     CIRCLE-ACTAB    ; q...$71
                fdb     GPRINT-ACTAB    ; r...$72
                opt     -l
                ttl     circle          ; plotting algorithm table
                opt     l
                pag
CIRTAB          fdb     75
                fcb     2,9,5           ; RADIUS OF CIRCLE / SHIFT COUNTER VALUES
                fdb     300
                fcb     3,12,7          ; THE FDB STATEMENT GIVES THE RADIUS OF THE CIRCLE
                fdb     1250
                fcb     4,15,9          ; AND THE FCC STATEMENTS GIVE THE VALUES OF
                fdb     5000
                fcb     5,18,11         ; N, (3N+3) AND (2N+1) RESPECTIVELY WHICH ARE THE
                fdb     32767
                fcb     6,21,13         ; NUMBERS OF SHIFTS USED BY CIRCLE PLOTTING ALGORITHM
                opt     -L
                ttl     colour          ; mask tables
                opt     l
                pag
********************************************************************************
**
** W A R N I N G * * *   THESE COLOUR MASK TABLES ARE HARDWARE DEPENDENT
**
********************************************************************************

*
*COLOUR MASK TABLES
*
CMASK4          fcb     %00000000       ; 2nd BYTE %00000000
                fcb     %00001000       ; 16 COLS %00000000
                fcb     %10000000       ; %00000000
                fcb     %10001000       ; %00000000 COLOUR MASK TABLE FOR MODES
                fcb     %00000000       ; %00001000 USING 4 POINTS PER BYTE
                fcb     %00001000       ; %00001000
                fcb     %10000000       ; %00001000
                fcb     %10001000       ; %00001000
                fcb     %00000000       ; %10000000
                fcb     %00001000       ; %10000000
                fcb     %10000000       ; %10000000
                fcb     %10001000       ; %10000000
                fcb     %00000000       ; %10001000
                fcb     %00001000       ; %10001000
                fcb     %10000000       ; %10001000
                fcb     %10001000       ; %10001000

CMASK8          fcb     %00000000       ; 2nd BYTE %00000000
                fcb     %10000000       ; (4 COL) %00000000 COLOUR MASK TABLES FOR MODES USING
                fcb     %00000000       ; %10000000 8 POINTS PER BYTE
                fcb     %10000000       ; %10000000
                fcb     %00000000       ; DUMMY ENTRIES TO ALLOW
                fcb     %10000000       ; WRAP ROUND OF COLOURS IF COLOUR NUMBER
                fcb     %00000000       ; IS > NO. OF COLOURS ALLOWED
                fcb     %10000000
                fcb     %00000000
                fcb     %10000000
                fcb     %00000000
                fcb     %10000000
                fcb     %00000000
                fcb     %10000000
                fcb     %00000000
                fcb     %10000000
                opt     -l
                ttl     CRT             ; initialisation tables
                opt     l
                pag
********************************************************************************
**
** W A R N I N G  * * *   THESE CRT TABLES ARE HARDWARE DEPENDENT
**
********************************************************************************
CRTTAB1         fcb     56
                fcb     40              ; 320X256 4 COLOUR MODE
                fcb     43
                fcb     $3A
                fcb     38
                fcb     0
                fcb     32
                fcb     34
                fcb     0
                fcb     7
                fcb     $20
                fcb     0
                spc     2
CRTTAB2         fcb     111
                fcb     80              ; 320X256 16 COLOUR MODE
                fcb     89
                fcb     $3A
                fcb     38
                fcb     0
                fcb     32
                fcb     34
                fcb     0
                fcb     7
                fcb     $20
                fcb     0
                spc     2
CRTTAB3         fcb     56
                fcb     40              ; 640X512 2 COLOUR MODE
                fcb     43
                fcb     $3A
                fcb     76
                fcb     0
                fcb     64
                fcb     68
                fcb     3
                fcb     6
                fcb     $20
                fcb     0
                spc     2
CRTTAB4         fcb     111
                fcb     80              ; 640X512 4 COLOUR MODE
                fcb     89
                fcb     $3A
                fcb     76
                fcb     0
                fcb     64
                fcb     68
                fcb     3
                fcb     6
                fcb     $20
                fcb     0
                opt     -l
                ttl     parameter       ; acquisition
                opt     l
                pag
*
*GPARMS SUBROUTINE TO GET A LIMITED NUMBER OF
*       INTEGERS (UP TO 4) AND STORE THEM
*       IN A BUFFER, TOGETHER WITH THE NUMBER
*       OF INTEGERS
*       (A) REG CONTAINS BYTE PASSED
*
GPARMS          leax    GPM10,PCR       ; SET RETURN VECTORS
                leay    GPM210,PCR
                lbra    ADUMP           ; DUMP AND/OR ABORT
GPM10           ldx     BUFPON          ; GET BUFFER POINTER
                inc     INTFLAG         ; SET INTEGER FLAG
                ldb     BINARY          ; ASCII OR BINARY ?
                beq     GPM12
                lbsr    GETINT          ; ASCII
                bra     GPM14
GPM12           lbsr    BGETINT         ; BINARY
GPM14           ldb     INTFLAG         ; INTEGER FLAG SET?
                beq     GPM20           ; NO...SKIP
                rts                     ; YES STILL GETTING INTEGER
GPM20           ldb     ESCFLAG         ; IS THIS THE NUMBER OF
                cmpb    #2              ; INTEGERS EXPECTED?
                bne     GPM200          ; NO...SKIP
                ldd     -2,X            ; YES...GET NUMBER OF INTEGERS
                beq     GPM205          ; SKIP IF NO. OF INTEGERS =0
                cmpd    #4              ; IS IT >4 OR <0 ?
                bhi     GPM100
                std     CCOUNT          ; ELSE STORE AS COUNTER
                inc     ESCFLAG         ; INCREMENT ESCAPE FLAG
                clra                    ; ENSURE CARRY IS CLEAR
                rts                     ; AND RETURN

GPM100          lslb                    ; NUMBER OF BYTES TO DUMP IS
                rola                    ; 2*NUMBER OF PARAMETERS FOR
                std     CCOUNT          ; BINARY DATA
                clra
                ldb     EOSFLAG         ; IS EOS FLAG SET?
                lbne    PABORT          ; YES...ABORT
                inc     DMPFLAG         ; NO SET DUMP FLAG
                inc     ABFLAG          ; AND SET ABORT FLAG
                rts

GPM200          ldd     CCOUNT
                subd    #1
                std     CCOUNT          ; DECREMENT COUNTER
                bne     GPM300          ; COUNTER=0 NO...SKIP
GPM205          ldb     EOSFLAG         ; YES...END OF SEQUENCE CODE RECEIVED
                beq     GPM350          ; NO...SKIP
GPM210          clrb                    ; YES CLEAR EOS FLAG
                stb     EOSFLAG         ; AND ESCFLAG
                stb     ESCFLAG
                leax    BUFFER,U
                leay    ACTAB,PCR       ; GET ACTION VECTOR
                lda     ACTION
                ldd     A,Y
                jmp     D,Y             ; JUMPTHRO' ACTION VECTOR

GPM300          ldb     EOSFLAG         ; END OF SEQUENCE CODE RECEIVED
                lbne    PABORT          ; YES...ABORT
                rts                     ; ELSE RETURN FOR NEXT INTEGER

GPM350          inc     DMPFLAG         ; SET DUMP FLAG AND RETURN
                rts
                opt     -l
                ttl     parameter       ; acquisition for PLOT
                opt     l
                pag
*
*PLOTPAIR - GETS A PAIR OF INTEGERS AND
*           STORES IN BUFFER FOR PLOT ROUTINE
*         - (A) REG CONTAINS BYTE PASSED
*
PLOTPAIR        leax    PLP10,PCR       ; SET RETURN VECTORS
                leay    PLP310,PCR
                lbra    ADUMP           ; DUMP AND/OR ABORT
PLP10           ldx     BUFPON          ; GET BUFFER POINTER
                inc     INTFLAG         ; INCREMENT INTEGER FLAG
                ldb     BINARY          ; ASCII OR BINARY ?
                beq     PLP12
                lbsr    GETINT          ; ASCII
                bra     PLP14
PLP12           lbsr    BGETINT         ; BINARY
PLP14           ldb     INTFLAG         ; IS INTEGER FLAG SET
                beq     PLP20           ; NO...SKIP
                rts                     ; YES..STILL GETTING INTEGER

PLP20           ldb     ESCFLAG         ; IS ESCAPE FLAG=2
                cmpb    #2
                bne     PLP200          ; NO...SKIP
                ldd     -2,X            ; YES...GET NUMBER OF INTEGERS
                lsra
                rorb                    ; DIVIDE BY 2
                bcc     PLP100
                ldd     -2,X
                bra     GPM100          ; DUMP &ABORT IF ODD
PLP100          leax    BUFFER,U        ; POINT TO PARAMETER BUFFER
                std     CCOUNT          ; STORE AS COUNTER
                beq     PLP110          ; SKIP IF COUNTER =0
                inc     ESCFLAG         ; ELSE INCREMENT ESCAPE FLAG (NOW 3)
                bra     PLP220

PLP110          std     ,X++            ; STORE IN BUFFER
                ldd     #1
                std     CCOUNT          ; SET COUNTER TO 1
                bra     PLP210

PLP200          ldb     COUNT           ; IS PAIR COUNTER =0
                beq     PLP210          ; YES...SKIP
                dec     COUNT           ; YES...DECREMENT PAIR COUNT
                rts                     ; GET ANOTHER INTEGER
PLP210          leax    BUFFER,U        ; POINT TO START OF PARAMETER BUFFER
                lbsr    PLOT            ; PLOT LINE
                bcc     PLP215
                inc     ABFLAG          ; ERROR...SKIP
                bra     PLP300
PLP215          ldd     CCOUNT          ; DECREMENT COUNTER
                subd    #1
                std     CCOUNT
                beq     PLP300          ; SKIP IF COUNTER=0
PLP220          ldb     EOSFLAG         ; END OF SEQUENCE RECEIVED
                lbne    PABORT          ; YES...ABORT
                inc     COUNT           ; SET PAIR COUNTER TO 1
                ldd     #2              ; NO
                leax    BUFFER,U        ; STORE 2 AT START OF BUFFER
                std     ,X++
                stx     BUFPON          ; SAVE BUFFER POINTER
                rts

PLP300          lda     EOSFLAG         ; END OF SEQUENCE RECEIVED?
                bne     PLP310          ; YES...SKIP
                inc     DMPFLAG         ; NO...SET DUMP FLAG
                tst     BINARY          ; ASCII OR BINARY DATA ?
                bne     PLP305          ; SKIP IF ASCII
                ldd     CCOUNT          ; IF BINARY THEN
                subd    #1              ; NUMBER OF BYTES TO DUMP IS
                lslb                    ; 4*(CURRENT VALUE OF PAIRS - 1)
                rola
                lslb
                rola
                std     CCOUNT
PLP305          clra                    ; CLEAR CARRY FOR DUMP
                rts
PLP310          clra
                sta     COUNT           ; CLEAR PAIR COUNTER
                sta     EOSFLAG         ; CLEAR EOS FLAG
                sta     ESCFLAG         ; ESCAPE FLAG
                sta     ABFLAG          ; IN CASE ERROR AFTER LAST PARAMETER
                sta     PLFLAG          ; IN CASE DUMP FROM 'DRAW'
                tst     ERROR           ; SYSTEM ERROR ?
                beq     PLP320          ; NO...SKIP
                ldb     ERROR
                sta     ERROR
                coma                    ; SET CARRY
                rts                     ; AND RETURN

PLP320          tst     OVRFLW          ; OVERFLOW ERROR ?
                beq     PLP330          ; NO..THEN RETURN
                ldb     #ERROR2         ; OVERFLOW ERROR CODE
                sta     OVRFLW
                coma
PLP330          rts
                opt     -l
                ttl     parameter       ; acquisition for DRAW
                opt     l
                pag
*
*DRAWPAIR - GETS A CHARACTER/INTEGER PAIR
*           OF PARAMETERS AND STORES IN
*           BUFFER - ALWAYS AN ASCII STRING
*
DRAWPAIR        leax    DP100,PCR       ; SET RETURN VECTORS
                leay    >PLP310,PCR
                lbra    ADUMP
DP100           ldx     BUFPON          ; GET BUFFER POINTER
                ldb     ESCFLAG         ; IS ESCAPE FLAG=2
                cmpb    #2
                beq     DP300           ; GET NUMBER OF PARAMETERS
                cmpb    #3
                bne     DP101
                inc     CHRFLAG         ; NO...SET CHARACTER FLAG
                leax    1,X
                stx     BUFPON          ; SAVE BUFFER POINTER
DP101           inc     INTFLAG         ; SET INTEGER FLAG
                lbsr    CHARINT         ; ASCII
                ldb     INTFLAG         ; IS INTEGER FLAG SET
                beq     DP110           ; NO...SKIP
                rts                     ; YES...STILL GETTING INTEGER
DP110           leax    BUFFER,U        ; POINT TO START OF PARAMETER BUFFER
                lbsr    DRAW            ; DRAW LINE
                bcc     DP115
                inc     ABFLAG          ; ERROR ?
                inc     PLFLAG          ; INDICATE ASCII DUMP REQUIRED
                bra     PLP300
DP115           ldb     EOSFLAG         ; NO...IS EOS RECEIVED
                bne     PLP310          ; YES...ABORT
                leax    BUFFER+1,U      ; POINT AT START OF BUFFER
                stx     BUFPON          ; SAVE BUFFER POINTER
                rts

DP300           inc     INTFLAG         ; SET INTEGER FLAG
                ldb     BINARY          ; ASCII OR BINARY ?
                beq     DP302
                lbsr    GETINT          ; ASCII
                bra     DP304
DP302           lbsr    BGETINT         ; BINARY
DP304           ldb     INTFLAG         ; STILL GETTING INTEGER ?
                beq     DP310           ; NO..SKIP
                rts

DP310           ldd     ,--X
                stx     BUFPON          ; SAVE BUFFER POINTER
                cmpd    #1              ; IS NUMBER OF PARAMETERS=1
                bne     DP320           ; NO...ERROR
                inc     ESCFLAG         ; NOW = 3
                rts

DP320           inc     PLFLAG          ; INDICATE ASCII DUMP IS REQUIRED FOR
                lbra    GPM100          ; 'DRAW' STRING ALWAYS
                opt     -l
                ttl     parameter       ; acquisition for GPRINT
                opt     l
                pag
*
* SUBROUTINE TEXTSTR
*
*    TO PASS STRINGS TO BE PRINTED ON THE GRAPHICS SCREEN FROM THE SCREEN
*    DRIVER TO THE GPRINT SUBROUTINE. THE STRING CHARACTERS ARE BUFFERED
*    INTO BLOCKS OF 10, OR UP TO A CARRIAGE RETURN BEFORE BEING PASSED TO
*    GPRINT.
*
* INPUT  :  (A) CONTAINS NEXT CHARACTER IN PRINT STRING
*

**
** NAME OF CHARACTER SET MODULE
**
CHRSET          fcs     'CHRSET'
**

TEXTSTR         leax    TX100,PCR       ; SET RETURN VECTOR
                lbra    ADUMP

TX100           ldx     BUFPON          ; GET CURRENT BUFFER POINTER
                ldb     ESCFLAG         ; IS THIS PART OF TEXT STRING ?
                cmpb    #4
                bne     TX400           ; NO...SKIP

                cmpa    #EOSEQ          ; CHECK FOR VALID CHARACTER.
                beq     TX300           ; IT MUST BE A PRINTABLE ASCII
                cmpa    #CONT1          ; CHARACTER (BETWEEN $20 AND $7F)
                lbls    TX600           ; OR A CARRIAGE RETURN ($0D)
                cmpa    #CONT2
                blo     TX310

TX300           clr     ESCFLAG         ; INDICATE END OF STRING IF CARRIAGE RETURN
TX310           sta     ,X+             ; STORE IN BUFFER AND
                stx     BUFPON          ; SAVE BUFFER POINTER

                tst     ESCFLAG         ; END OF STRING ?
                lbeq    GPRINT          ; YES... THEN PRINT REMAINING CHARACTERS
                dec     COUNT           ; NO...THEN SEE IF BUFFER IS FULL
                lbeq    GPRINT          ; PRINT FULL BUFFER
                clra                    ; ENSURE CARRY CLEAR
                rts                     ; OR GET NEXT CHARACTER

TX400           clrb                    ; ENSURE CARRY CLEAR
                inc     INTFLAG         ; SET 'GETTING INTEGER' FLAG
                ldb     BINARY          ; ASCII OR BINARY DATA ?
                beq     TX410
                lbsr    GETINT          ; GET ASCII INTEGER
                bra     TX420
TX410           lbsr    BGETINT         ; GET BINARY INTEGER
TX420           ldb     INTFLAG         ; STILL GETTING INTEGER ?
                beq     TX430           ; NO...SKIP
                rts                     ; YES...CONTINUE

TX430           ldb     ESCFLAG
                cmpb    #3              ; IS THIS BACKGROUND COLOUR
                beq     TX500           ; YES...SKIP
                ldd     ,--X            ; GET NUMBER OF PARAMETERS
                beq     TX750           ; MUST BE 1 OR 2
                cmpd    #2
                bhi     TX750           ; OR ELSE ERROR
                beq     TX450           ; =2 THEN SKIP
                inc     ESCFLAG         ; INDICATE ONLY 1 PARAMETER
                lda     BGCOL           ; BACKGROUND COLOUR IS COLOUR NOT TO PLOT
                cmpa    DRCOL           ; UNLESS DRAW COLOUR=BACKGROUND COLOUR
                bne     TX440           ; IN WHICH CASE MAKE DUMMY BACKGROUND COLOUR
                inca                    ; AS COLOUR NOT TO PLOT
                anda    #$0F            ; WRAP ROUND IF >15
TX440           sta     GPBGCOL         ; SAVE AS BACKGROUND COLOUR FOR GPRINT
                sta     NPCOL           ; AND COLOUR NOT TO PLOT

TX450           inc     ESCFLAG         ; NOW=3 OR 4, HAVING GOT NO. OF PARAMS.
                stx     BUFPON          ; BUFFER POINTER AT START OF BUFFER
                lda     #BCOUNT         ; BUFFER SIZE = 10 BYTES
                sta     COUNT           ; SAVE AS BUFFER COUNTER

                pshs    U               ; NOW LINK TO CHARACTER SET MODULE
                ldu     >D.Proc
                pshs    U
                ldu     >D.SysPrc
                stu     >D.Proc
                leax    CHRSET,PCR
                lda     #MODLANG
                os9     F$Link
                sty     LINKENT         ; SAVE ENTRY POINT AND
                stu     LINKADR         ; START OF MODULE ADDRESS
                puls    U
                stu     >D.Proc
                puls    U
                bcs     TX700           ; SKIP IF ERROR
                inc     GPTFLAG         ; SET GPRINT FLAG
                rts                     ; ELSE GO AND GET TEXT STRING

TX500           ldd     ,--X            ; GET REQUIRED BACKGROUND COLOUR
                stx     BUFPON
                cmpd    #MAXCOL
                bhi     TX600           ; MUST BE IN RANGE 0-15
                stb     GPBGCOL         ; SAVE AS BACKGROUND COLOUR FOR GPRINT
                lda     #$FF            ; DUMMY COLOUR FOR NOPLOT
                sta     NPCOL
                inc     ESCFLAG         ; NOW=4 READY TO GET TEXT
                clra                    ; ENSURE CARRY CLEAR
                rts

TX600           pshs    U               ; NON VALID CHARACTER...UNLINK
                ldu     >D.Proc
                pshs    U
                ldu     >D.SysPrc
                stu     >D.Proc
                ldu     LINKADR         ; CHARACTER SET MODULE
                os9     F$Unlink
                puls    U
                stu     >D.Proc
                puls    U
                bcc     TX750
TX700           stb     ERROR           ; SAVE SYSTEM ERROR CODE
TX750           inc     DMPFLAG         ; PARAMETER DUMP AND
                inc     ABFLAG          ; ABORT REQUIRED.
                inc     PLFLAG          ; ***ALWAYS ASCII DUMP***
                clr     COUNT
                clr     GPTFLAG
                rts
                opt     -l
                ttl     build           ; binary integer from decimal ASCII codes
                opt     l
                pag
*
*GETINT ACCEPTS BYTE BY BYTE ASCII CODES
*       AND BUILDS A SIGNED INTEGER OR
*       RECOGNISES A PARAMETER ERROR
*       *CHARINT USES SOME OF THIS CODE IN
*       COMMON WITH GETINT
*       (A) REG CONTAINS BYTE PASSED
*
GETINT          ldb     NUMFLAG         ; NUMBER FLAG SET?
                lbeq    GI300           ; NO...SKIP
                cmpa    #C$Spac         ; IS CHARACTER A SPACE
                bne     GI200           ; NO...SKIP
GI50            cmpb    #1
                beq     GI270
                inc     SPFLAG          ; SET SPACE FLAG
                rts
GI200           cmpa    #'.             ; IS IT A DECIMAL POINT
                bne     GI210           ; NO SKIP
                inc     DPFLAG          ; YES SET DECIMAL POINT FLAG
GI210           cmpa    #EOSEQ          ; IS IT CARRIAGE RETURN
                beq     GI220           ; YES...SKIP
                cmpa    #',             ; IS IT COMMA
                beq     GI230           ; YES...SKIP
                cmpa    #';             ; IS IT SEMI-COLON
                beq     GI230           ; YES...SKIP
                ldb     SPFLAG          ; IS SPACE FLAG SET
                bne     GI270           ; YES...SKIP
                ldb     DPFLAG          ; IS DECIMAL POINT FLAG SET
                beq     GI215           ; NO...SKIP
                rts                     ; YES...RETURN
GI215           cmpa    #'0             ; DOES CODE LIE BETWEEN
                blo     GI270           ; CODE FOR 0 AND CODE
                cmpa    #'9             ; FOR 9 SKIP IF NOT
                bhi     GI270

                tfr     A,B             ; GET INTO LSB
                clra
                subb    #$30            ; CONVERT ASCII TO NUMBER
                tst     OOR             ; OUT OF RANGE ?...SKIP
                beq     GIA
                bra     GI270
GIA             addd    N1
                bpl     GIB
                bra     GI270           ; OUT OF RANGE...SKIP
GIB             std     N2              ; STORE AS N2
                lslb                    ; MULTIPLY N1 BY 10
                rola
                bpl     GIC
                inc     OOR
GIC             std     RES             ; TEMP RESULT
                lslb
                rola
                bpl     GID
                inc     OOR
GID             lslb
                rola
                bpl     GIE
                inc     OOR
GIE             addd    RES
                bpl     GIF
                inc     OOR
GIF             std     N1              ; N1=INTEGER X10
GI216           inc     NUMFLAG         ; INCREMENT NUMBER FLAG
                rts

GI220           inc     EOSFLAG         ; SET END OF SEQUENCE FLAG
GI230           cmpb    #2              ; NUMBER FLAG >=2
                bge     GI290           ; NO...SKIP
GI270           inc     DMPFLAG         ; NO...SET DUMP FLAG
                inc     ABFLAG          ; SET ABORT FLAG
                clr     OOR
                rts

GI290           clrb
                stb     SPFLAG          ; CLEAR SPACE FLAG
                stb     DPFLAG          ; DECIMAL POINT FLAG
                stb     NUMFLAG         ; NUMBER FLAG
                ldd     N2              ; GET INTEGER
                tst     ASIGN           ; IS IT -VE
                beq     GI295           ; NO...SKIP
                coma
                comb
                addd    #1
GI295           std     ,X++            ; STORE INTEGER IN BUFFER
                stx     BUFPON          ; SAVE BUFFER POINTER
                ldd     #0
                std     N1              ; CLEAR N1
                stb     OOR
                stb     ASIGN           ; CLEAR SIGN FLAG
                stb     INTFLAG         ; CLEAR GETTING INTEGER FLAG
                rts
GI300           cmpa    #C$Spac         ; IS IT SPACE
                bne     GI310           ; NO...SKIP
                rts                     ; YES...RETURN
GI310           cmpa    #'-             ; IS IT MINUS SIGN
                bne     GI320           ; NO...SKIP
GI315           inc     ASIGN           ; YES..SET SIGN FLAG
                bra     GI216
GI320           cmpa    #'+             ; IS IT PLUS SIGN
                beq     GI216           ; YES...SKIP
GI330           inc     NUMFLAG
                bra     GI215
                opt     -l
                ttl     binary          ; integer acquisition
                opt     l
                pag
*
*BGETINT   BINARY VERSION OF GETINT
*
* IN   EXPECTS BYTE IN (A) REG
*
BGETINT         ldb     NUMFLAG         ; 1ST HALF OF INTEGER ?
                bne     BT10            ; NO...SKIP
                sta     RES
                inc     NUMFLAG
                rts

BT10            tfr     A,B             ; YES
                lda     RES             ; GET INTEGER IN D
                clr     NUMFLAG
                clr     INTFLAG
                std     ,X++            ; STORE IN BUFFER
                stx     BUFPON          ; SAVE BUFFER POINTER
                ldb     ESCFLAG         ; IS THIS NO. OF INTEGERS ?
                cmpb    #2
                bne     BT20            ; NO...SKIP
                ldd     -2,X            ; YES...IS NO OF INTEGERS=0
                beq     BT30            ; YES..SET EOS FLAG
                rts                     ; NO...RETURN
BT20            ldd     CCOUNT          ; IS THIS THE LAST INTEGER OR LAST PAIR
                cmpd    #1
                bne     BT40            ; NO...RETURN
                ldb     COUNT           ; IF LAST PAIR - IS IT LAST OF PAIR ?
                bne     BT40            ; NO...RETURN
BT30            inc     EOSFLAG         ; YES...SET EOSFLAG
BT40            rts
                opt     -l
                ttl     acquisition     ; of ASCII coded character / integer pair
                opt     l
                pag
*
*CHARINT - GETS CHARACTER/INTEGER PAIR AND
*          PUTS THEM IN BUFFER
*        - (X) REG POINTS TO PARAMETER BUFFER
*           STRING
*        - (A) REG CONTAINS ASCII BYTE OF CHAR/INT
CHARINT         ldb     CHRFLAG         ; IS CHR FLAG SET?
                beq     CH100           ; NO...SKIP
                inc     ESCFLAG         ; NOW 4
                cmpa    #C$Spac         ; YES...IS IT SPACE?
                bne     CH10            ; NO...SKIP
                rts                     ; YES...RETURN
CH10            clr     CHRFLAG         ; CLEAR CHARACTER FLAG
                cmpa    #'D             ; IS IT 'D'
                beq     CH40            ; YES...SKIP
                cmpa    #'M             ; IS IT 'M'?
                beq     CH40            ; YES...SKIP
                cmpa    #'T             ; IS IT 'T'?
                bne     CH220           ; NO..THEN ERROR

CH40            sta     COUNT           ; STORE IN TEMPORARY LOCATION
                rts

CH100           ldb     NUMFLAG         ; IS NUMBER FLAG SET
                bne     CH200           ; YES...SKIP
                cmpa    #C$Spac         ; NO...IS IT SPACE?
                bne     CH120
                rts
CH120           cmpa    #'-             ; NO...IS IT MINUS SIGN
                beq     GI315           ; YES...NOW AS GETINT
                bra     GI330           ; AND NOW AS GETINT

CH200           cmpa    #C$Spac         ; IS IT SPACE
                lbeq    GI50            ; NOW AS GETINT
                cmpa    #EOSEQ          ; IS IT END OF SEQUENCE CODE
                beq     CH205           ; NOW AS GETINT
                cmpa    #'D             ; IS IT 'D' 'M' OR 'T'
                beq     CH210           ; IF SO SKIP
                cmpa    #'M
                beq     CH210
                cmpa    #'T
                beq     CH210
                ldb     SPFLAG          ; IS SPACE FLAG SET
                bne     CH220           ; YES...SKIP
                lbra    GI215           ; NOW AS GETINT
CH205           inc     EOSFLAG         ; END OF SEQUENCE CODE RECEIVED
CH210           ldb     COUNT           ; GET CHARACTER
                stb     BUFFER          ; STORE AT START OF BUFFER
                sta     COUNT           ; STORE NEXT CHARACTER IN TEMP LOCATION
                ldb     NUMFLAG         ; GET NUMBER FLAG
                lbra    GI230
CH220           inc     PLFLAG          ; INDICATE ASCII DUMP ALWAYS REUIRED FOR
                lbra    GI270           ; 'DRAW' STRING
                opt     -l
                ttl     check           ; for parameter dump
                opt     l
                pag
*
*ADUMP    -CHECK FOR ABORT OR PARAMETER
*          DUMP WHILE GETTING INTEGER
*
ADUMP           ldb     ESCFLAG         ; FIRST TIME IN ESCAPE SEQ. ?
                cmpb    #1
                bne     AD10            ; N0...SKIP
                inc     ESCFLAG         ; YES...ESCAPE FLAG NOW = 2
                rts

AD10            ldb     DMPFLAG
                bne     AD20
                jmp     ,X

AD20            tst     PLFLAG          ; 'DRAW' ALWAYS REQUIRES ASCII DUMP
                bne     AD25
                tst     BINARY          ; ASCII OR BINARY DATA ?
                bne     AD25            ; SKIP IF ASCII
                ldd     CCOUNT          ; ELSE JUNK BYTE AND DECREMENT COUNTER
                subd    #1
                std     CCOUNT
                beq     AD30            ; ALL DONE ? YES...SKIP
                rts                     ; NO...RTEURN
AD25            cmpa    #EOSEQ          ; IS IT END OF SEQ. CHARACTER
                beq     AD30            ; YES...SKIP
                clra
                rts                     ; AND RETURN

AD30            clrb
                stb     DMPFLAG         ; RESET FLAGS
                stb     INTFLAG
                stb     SPFLAG
                ldb     ABFLAG          ; IS ABORT REQUIRED ?
                bne     PABORT
                jmp     ,Y              ; NO...VECTOR BACK
                opt     -l
                ttl     clean           ; up routines if aborting
                opt     l
                pag
*
*PABORT - CLEAR ALL FLAGS CONNECTED WITH GETTING
*         PARAMETERS AND ABORT
*
PABORT          clrb                    ; CLEAR B
                stb     COUNT
                stb     CCOUNT
                stb     CCOUNT+1
                stb     EOSFLAG
                stb     NUMFLAG
                stb     ABFLAG
                stb     ASIGN
                stb     DPFLAG
                stb     PLFLAG          ; IN CASE OF 'DRAW' ABORT
*
* FALL THROUGH TO ABORT
*
                spc     5
*
*ABORT - SETS CARRY AND PUTS ERROR
*        CODE 56 - PARAMETER ERROR - IN
*        (B) REGISTER
*
ABORT           clr     ESCFLAG         ; CLEAR ESCAPE FLAG
                clr     DFLAG           ; CLEAR DEFAULT FLAG
                ldb     #ERROR1         ; PARAMETER ERROR
                tst     ERROR           ; OR IS IT SYSTEM ERROR ?
                beq     AB10            ; NO...CHECK FOR OVERFLOW ERROR
                ldb     ERROR
                clr     ERROR
                bra     AB20            ; RETURN

AB10            tst     OVRFLW
                beq     AB20            ; IS IT OVERFLOW ERROR NO..SKIP
                ldb     #ERROR2         ; GET ERROR CODE
                clr     OVRFLW

AB20            coma
                rts
                opt     -l
                ttl     graphics        ; mode request
                opt     l
                pag
*
* SUBROUTINE GMODE
*
*   SELECTS NUMBER OF PAGES,RESOLUTION & COLOUR MASK TABLE
*
* INPUT  : (X) POINTS TO START OF PARAMETER BUFFER
*
* OUTPUT : STATIC STORAGE LOCATIONS SET
*
GMODE           ldd     ,X++            ; GET NUMBER OF PARAMETERS
                lbeq    ABORT1
                cmpb    #2
                lbhi    ABORT1
                beq     GM100
                ldd     #1
                std     2,X

GM100           ldd     ,X
                bne     GM200
                ldd     2,X
                bne     ABORT1
                ldd     MEMSTRT         ; DOES ANY GRAPHICS MEMORY EXIST ?
                beq     GM150           ; NO...SKIP
                lbsr    RETMEM
                bcs     ABORT1
                clra
                clrb
                sta     RESOL           ; CLEAR VITAL FLAGS
                stb     BYTES
                stb     GPAGES
                std     LBADDR
GM150           rts

GM200           ldd     MEMSTRT         ; RETURN ANY ALREADY EXISTING GRAPHICS MEMORY
                beq     GM205           ; SKIP IF NO GRAPHICS MEMORY
                pshs    X
                lbsr    RETMEM          ; RETURN CURRENT GRAPHICS MEMORY
                puls    X
                bcs     ABORT1
GM205           ldd     2,X             ; CHECK NUMBER OF PAGES REQUESTED IS
                beq     ABORT1          ; IN RANGE 1-4
                cmpd    #MAXPAG
                bhi     ABORT1
                ldd     ,X              ; CHECK THAT MODE REQUESTED IS IN
                cmpd    #4              ; RANGE 1-4 (ALREADY CHECKED FOR MODE 0)
                bhi     ABORT1

                leay    GPAGES,U
                lda     #PBLOCK         ; CLEAR GRAPHICS PARAMETER SPACE
                clrb
GMLOOP          stb     ,Y+
                deca
                bne     GMLOOP
                ldd     #SCAL0          ; SET SCALE FACTORS TO 1x256
                std     XSCALE
                std     YSCALE
                ldd     #COS0           ; SET COSINES TO 1x16384
                std     COSTH
                std     HCOS

                leay    CMASK8,PCR      ; COLOUR MASK TABLE USED IN THREE OF THE FOUR MODES.
                lda     #FACT2
                sta     BYTFAC
                ldd     #FACT4
                std     BYTCONST
                ldd     #OFFSCRN
                std     LASTX           ; DUMMY VALUES OF LAST POINT PLOTTED
                std     LASTY           ; FOR USE IF PMODE=2

                ldd     ,X
                cmpb    #MODE4
                bne     GM210           ; IF P1<>4...SKIP
                ldd     2,X             ; CHECK FOR VALID NUMBER OF PAGES
                cmpb    #PAGM4
                bne     ABORT1
                stb     GPAGES
                inc     BYTES           ; NOW = 1 INDICATING 80 BYTE PAIRS ACROSS SCREEN
                lda     #FACT3
                sta     BYTFAC
                ldd     #FACT5
                std     BYTCONST
                leax    CRTTAB4,PCR     ; POINT TO CRT TABLE FOR THIS MODE
                bra     GM215

ABORT1          lbra    ABORT

GM210           cmpb    #MODE3          ; IS P1=3?
                bne     GM220           ; IF NOT SKIP
                ldd     2,X             ; CHECK FOR VALID NUMBER OF PAGES
                cmpb    #PAGM2.3
                bgt     ABORT1
                stb     GPAGES
                inc     ONEBYTE
                leax    CRTTAB3,PCR     ; POINT TO CRT TABLE FOR THIS MODE
GM215           ldb     #HIRES
                stb     RESOL
                ldd     #HIXmax         ; SET MAX X AND Y
                std     XMAX
                ldd     #HIYmax
                std     YMAX
                bra     GM250

GM220           cmpb    #MODE2          ; IS P1=2?
                bne     GM230           ; IF NOT SKIP
                ldd     2,X             ; CHECK FOR VALID NUMBER OF PAGES
                cmpb    #PAGM2.3
                bgt     ABORT1
                stb     GPAGES
                leay    CMASK4,PCR      ; PONT TO COLOUR MASK TABLE FOR THIS MODE
                inc     BYTES           ; NOW=1 INDICATING 80 BYTE PAIRS ACROSS SCREEN
                lda     #FACT3
                sta     BYTFAC
                ldd     #FACT5
                std     BYTCONST
                inc     COL16
                leax    CRTTAB2,PCR     ; POINT TO CRT TABLE FOR THIS MODE
                bra     GM235

GM230           cmpb    #MODE1          ; IS P1=1?
                bne     ABORT1          ; IF NOT SKIP
                ldd     2,X             ; CHECK FOR VALID NUMBER OF PAGES
                cmpb    #MAXPAG
                bgt     ABORT1
                stb     GPAGES
                leax    CRTTAB1,PCR     ; POINT TO CRT TABLE FOR THIS MODE
GM235           ldb     #LORES
                stb     RESOL
                ldd     #LOXmax         ; SET MAX X AND Y
                std     XMAX
                ldd     #LOYmax
                std     YMAX

GM250           stx     CRTENT          ; SAVE CRT TABLE ENTRY POINTER
                sty     TABENT          ; STORE COLOUR MASK TABLE ENTRY PONITER
                lda     15,Y            ; GET COLOUR MASK 15
                sta     GETMSK
*
* FALL THROUGH TO GINIT
*
                opt     -l
                ttl     initialisation  ; of static storage
                opt     l
                pag
*
* SUBROUTINE GINIT
*
*        -INITIALISE MEMORY BASE ADDRESS POINTER
*         TO (0,0) ON DRAW PAGE 1 AND DISPLAY PAGE 1 (DEFAULT)
*        -SET BACKGROUND COLOUR MASKS TO COLOUR 0
*        -SET FOREGROUND COLOUR MASKS TO COLOUR 1
*

GINIT           lbsr    GETMEM          ; GET REQUESTED MEMORY
                bcs     ABORT1          ; ABORT IF ERROR
                ldb     #2              ; )CALC CONST FOR PLOT
                bsr     BASADR          ; )
                subd    BYTCONST        ; )
                addd    MEMSTRT         ; )
                std     MCONST          ; )
                lda     RESOL
                cmpa    #LORES
                bhi     GT20            ; IF RESOLUTION >2 ie 640 x 512

                lda     BYTES           ; CALCULATE SHIFT MASKS AND SHIFT COUNTERS USED
                bne     GT10            ; IN MEMORY CALCULATIONS
                ldb     #SHMSK1
                bra     GT40

GT10            ldb     #SHMSK2
                bra     GT40

GT20            lda     BYTES
                bne     GT30
                ldb     #SHMSK3
                lda     #SHFTS
                bra     GT40

GT30            ldb     #SHMSK1
                clra                    ; NO SHIFTS REQD

GT40            sta     SHFLAG          ; STORE SHIFT FLAG
                stb     SHMSK           ; STORE SHIFT MASK
                comb
                stb     SHMSKX          ; STORE SHIFT MASK FOR USE IN MEMSHIFT
                lda     1,Y             ; COL 1 (Y CONTAINS COL. MSK TABLE ENTRY POINTER )
                sta     DCMSK1
                tst     ONEBYTE         ; 640X512 2 COLOUR MODE
                beq     GT50            ; DUPLICATE COLOUR MASK 1 AS MASK 2
                sta     DCMSK2

GT50            lda     #1
                sta     DRCOL
                leax    BUFFER,U        ; SELECT DEFAULT COLOUR SET
                ldd     #0              ; OF COLOURS 0,1,2,3
                std     ,X
                lbsr    CSET
                ldb     GPAGES          ; CLEAR ALL GRAPHICS PAGES

GTLOOP          pshs    B
                bsr     BASADR          ; TO DEFAULT BACKGROUND
                std     CGPAGE
                lbsr    CLEARPAG        ; COLOUR, AND LEAVE PAGE 1 AS
                puls    B
                lbcs    ABORT           ; ABORT IF ERROR
                decb                    ; CURRENT GRAPHICS DRAW PAGE
                bne     GTLOOP
                lbsr    DISPAG          ; SET DISPLAY PAGE TO 1
                lbra    PIASET          ; SETS PIA FOR COLOUR SET 1(DEFAULT)
                opt     -l
                ttl     graphics        ; memory base address manipulation
                opt     l
                pag
********************************************************************************
**
** W A R N I N G  * * *   THIS CALCULATION IS HARDWARE DEPENDENT
**
********************************************************************************

*
* SUBROUTINE BASADR
*
*   CALCULATES MEMORY ADDRESS (DIVIDED BY 2 TO BE IN 64K RANGE) OF SCREEN
*   POINT (0,Ymax) ON GIVEN PAGE RELATIVE TO START OF ALLOCATED MEMORY
*   THIS VALUE IS :- RESOLUTION X (PAGE NO.-1) X (BYTES+1) X 5120
*
* INPUT  : (B) CONTAINS PAGE NUMBER, RESOL & BYTES IN STATIC
*
* OUTPUT : (D) CONTAINS REQUIRED ADDRESS
*
BASADR          decb
                bne     BA10
                clra
                rts
BA10            lda     RESOL           ; RESOLUTION x PAGE <=6
                mul                     ; RESULT IN B ONLY
                lda     BYTES           ; RESOL X PAGES X BYTES <=12
                adda    #1
                mul                     ; IN B ONLY
                lda     #FACT1          ; 5120=160 x 32
                mul
                lslb
                rola                    ; CALCULATES
                lslb                    ; RESOLUTIONx(PAGE NO.-1)x10240
                rola
                lslb
                rola
                lslb
                rola
                lslb
                rola
                rts
                opt     -l
                ttl     request         ; graphics memory
                opt     l
                pag
*
*GETMEM - REQUESTS REQUIRED AMOUNT OF CONTINGUOUS
*         MEMORY FOR GRAPHICS MODE
*
*IN -    RESOL GPAGES CONTAIN INFORMATION
*         FOR CALCULATING AMOUNT OF MEMORY REQ'D
*       - BYTES FLAG TO INDICATE IF HIGHER
*         BYTE BLOCK IS REQUIRED
*

GETMEM          lda     RESOL
                ldb     GPAGES
                mul                     ; IN (B) ONLY (MAX VALUE=6)
                lda     BYTES
                adda    #1
                mul
                lda     #10
                mul                     ; (B) CONTAINS RESOLUTION X PAGES X 10
                lsrb                    ; (MAX VALUE=60)
                lsrb
                os9     F$GMAP          ; REQUEST CONTIGUOUS MEMORY
                bcs     GETM30          ; ANY ERROR
                tfr     X,D             ; RETURNS PHYSICAL BLOCK NUMBER
                exg     B,A             ; CONVERT TO PHYSICAL START
                lsla                    ; ADDRESS OF GRAPHICS MEMORY
                lsla
                lsla
                std     MEMSTRT         ; STORE VALUE
                ldb     #1              ; SET GRAPHICS MEMORY EXISTS FLAG
                stb     GFXFLAG
                rts

GETM30          stb     ERROR           ; SYSTEM ERROR
                rts
                opt     -l
                ttl     return          ; graphics memory
                opt     l
                pag
*
*RETMEM - REQUESTS CURRENTLY ALLOCATED
*         GRAPHICS MEMORY IS RELEASED
*
*IN     - RESOL GPAGES BYTES,U CONTAIN
*         INFORMATION TO CALCULATE AMOUNT OF MEMORY
*         IN USE
*       - MEMSTRT CONTAINS PHYSICAL START
*         ADDRESS
*
RETMEM          pshs    U
                ldu     >D.Proc
                pshs    U
                ldu     >D.SysPrc
                stu     >D.Proc
                lbsr    CLRBLOCK        ; UNMAP ANY MAPPED IN MEMORY
                puls    U
                stu     >D.Proc
                puls    U
                bcs     RETM10
                ldd     MEMSTRT
                exg     A,B
                lsrb
                lsrb
                lsrb
                tfr     D,X             ; (X) NOW CONTAINS PHYS. MEMORY START BLOCK
                lda     RESOL
                ldb     GPAGES
                mul
                lda     BYTES
                adda    #1
                mul
                lda     #10
                mul
                lsrb
                lsrb                    ; (B) NOW CONTAINS NO. OF 4K BLOCKS TO RETURN

                os9     F$GCLR          ; REQUEST MEMORY RELEASE
                bcs     RETM10
                ldd     #0              ; CLEAR MEMORY START ADDRESS
                std     MEMSTRT
                stb     GFXFLAG         ; CLEAR GRAPHICS MEMORY EXISTS FLAG
                rts

RETM10          stb     ERROR           ; SAVE SYSTEM ERROR CODE
                rts
                opt     -l
                ttl     set             ; up PIA
                opt     l
                pag
*
*PIASET   TO SET UP PIA FOR GRAPHICS MODE
*  IN     RESOL,U AND BYTES,U  ARE USED TO SELECT
*         REQUIRED BITS TO BE USED.
*
PIASET          ldy     #PIA            ; GET BASE ADDRESS OF PORT B
                pshs    CC              ; MASK INTERRUPTS
                orcc    #IntMasks
                ldb     >D.GRReg        ; GET PORT VALUE
                andb    #PIACLR1        ; SET/CLEAR REQUIRED BITS
                lda     RESOL
                cmpa    #2
                beq     PIA20
                tst     BYTES           ; SET UP PIA PORT B FOR REQUIRED
                beq     PIA10           ; RESOLUTION / MODE
                orb     #PIASET4
                bra     PIA40
PIA10           orb     #PIASET3
                bra     PIA40

PIA20           tst     BYTES
                beq     PIA30
                orb     #PIASET2
                bra     PIA40
PIA30           orb     #PIASET1

PIA40           tst     COL16
                bne     PIA50           ; COLOUR MAPPER NOT USED
                orb     #PIASET5        ; COLOUR MAPPER USED
PIA50           stb     >D.GRReg        ; SAVE PORT VALUE
                stb     ,Y
                puls    CC              ; CLEAR INTERRUPT MASKS
                rts
                opt     -l
                ttl     request         ; CRT controller to display graphics page
                opt     l
                pag
*
*DISPAG    -SETS UP CRTC CONTROLLER FO REQUIRED GRAPHICS MODE
*           AND PASSES  PHYSICAL ADDRESS OF START
*           OF PAGE FOR DISPLAY (BITS 15 -3)
*           TO CRTC CONTROLLER
*

DISPAG          ldx     CRTENT          ; POINT TO CORRECT CRTC TABLE
                clra
                ldb     #12
                bsr     T.CRTC

                ldd     CDPAGE          ; GET PHYS ADDR OF START OF PAGE
                addd    MEMSTRT
                lsra
                lsra                    ; POSITIONS 12-0
                lsra
                pshs    D
                leax    ,S              ; ADDRESS OF VAL+UES TO PUT IN
                lda     #12             ; START REGISTER
                ldb     #2              ; NO. OF REGISTERS
                bsr     T.CRTC          ; REWRITE REGISTERS
                leas    2,S
                ldb     #1
                stb     GFXDISP         ; GRAPHICS DISPLAY MODE FLAG
                rts
                opt     -l
                ttl     set             ; up CRT controller to graphics mode
                opt     l
                pag
*
* T.CRTC     TO INITIALISE CRTC CONTROLLER
*            OR TO CHANGE GRAPHICS DISPLAY PAGE
*  IN     (A)   START REGISTER NUMBER
*         (B)   NUMBER OF REGISTERS TO WRITE
*         (X)   POINTER TO DATA STORE
*
T.CRTC          pshs    CC              ; MASK INTERRUPTS
                orcc    #IntMasks
                pshs    Y,B
                ldy     #CRTC           ; GET CRTC ADDRESS
T.CRTC1         sta     ,Y              ; STORE REGISTER NUMBER
                ldb     ,X+
                stb     1,Y             ; STORE REGISTER VALUE
                inca                    ; INCREMENT REGISTER NUMBER
                dec     ,S              ; DECREMENT COUNTER
                bne     T.CRTC1
                puls    Y,B
                puls    CC              ; CLEAR INTERRUPT MASK
                rts
                opt     -l
                ttl     select          ; graphics draw page
                opt     l
                pag
*
*GPAGE - SELECT GRAPHICS PAGE TO DRAW INAND CALCULATES
*          BASE ADDRESS [OF (0,Ymax)] REL.TO START OF MEMORY
*      - (X) REG POINTS TO START OF PARAMETER BUFFER
*      - EXPECTS P1 INTEGER
*      - DEFAULTS TO 1 IF NOT GIVEN
*
GPAGE           lda     #1
                sta     PMIN            ; MIN VALUE FOR PARAMETER
                bsr     GET1D1          ; RETURNS P1 IN (B)
                bcs     ABORT2          ; ERROR...ABORT
                tsta    >256            ; ?
                bne     ABORT2          ; YES...ABORT
                cmpb    GPAGES          ; VALID PAGE ?
                bhi     ABORT2          ; NO...ABORT
                pshs    B               ; STACK PAGE NUMBER
                lbsr    BASADR          ; CALCULATE BASE ADDRESS
                std     CGPAGE          ; STORE VALUE
                puls    B               ; )
                addb    #1              ; )CALC CONST FOR PPLOT
                lbsr    BASADR          ; )
                subd    BYTCONST        ; )
                addd    MEMSTRT         ; )
                std     MCONST          ; )
                rts
                opt     -l
                ttl     GMAP            ; function
                opt     l
                pag
*
* SUBROUTINE GMAP
*
GMAP            rts
                opt     -l
                ttl     get             ; parameter from buffer, default to 1
                opt     l
*
*GET1D1 - GETS EXACTLY ONE PARAMETER OR SETS
*         DEFAULT VALUE OF 1
*       - (X) REG POINTS TO PARAMETER BUFFER
*       - EXPECTS P1 INTEGER
*       - SETS CARRY TO INDICATE DEFAULT USED
*       - RETURNS P1 IN (D) REG
*
GET1D1          clr     DFLAG           ; CLEAR DEFAULT FLAG
                ldd     ,X++            ; GET NO. OF PARAMETERS
                beq     GE100           ; SKIP IF 0
                cmpd    #1              ; IS IT 1?
                bne     GE200           ; IF NOT ABORT
                ldd     ,X              ; ELSE GET P1
                cmpb    PMIN
                blt     GE200           ; ABORT
                clr     PMIN
                rts
GE100           ldb     #1              ; SET DEFAULT
                inc     DFLAG           ; SET DEFAULT FLAG
                clr     PMIN
                rts
GE200           coma                    ; SET CARRY FOR ABORT
                clr     PMIN
                rts
                opt     -l
                ttl     selection       ; of colours to make up colour set
                opt     l
                pag
*
*CSET    -SELECTS COLOURS USED WHEN COLOUR
*         MAPPER IS ENABLED
*
*  IN   - (X) POINTS TO START OF PARAMETER BUFFER
*       - EXPECTS P1, P2, P3, P4 INTEGER
*
*
*
CSET            ldd     ,X++            ; GET NUMBER OF PARAMETERS
                bne     CS100           ; <>0...SKIP
                ldy     #0
CSLOOP1         sty     ,X++            ; DEFAULT VALUES P1=0
                leay    1,Y             ; P2=1
                cmpy    #4              ; P3=2
                bne     CSLOOP1         ; P4=3
                leax    -8,X            ; RESTORE BUFFER POINTER

CS100           lda     RESOL           ; 640x512 SCREEN ?
                cmpa    #LORES
                beq     CS300           ; NO...SKIP
                tst     BYTES           ; 2 COLOUR MODE ?
                bne     CS200           ; NO...SKIP
                ldd     -2,X            ; GET NO.OF PARAMS.
                beq     CS150           ; =0...SKIP
                cmpd    #2              ; =2 ?
                bne     ABORT2          ; NO...SKIP
CS150           ldd     ,X
                std     4,X             ; P3=P1
                ldd     2,X
                std     6,X             ; P4=P2
                bra     CS500
ABORT2          lbra    ABORT

CS200           ldd     -2,X            ; GET NO.OF PARAMS
                beq     CS500           ; =0 ? YES...SKIP
                cmpd    #4              ; =4 ?
                bne     ABORT2          ; NO...ABORT
                bra     CS500

CS300           lda     BYTES           ; 4 COLOUR MODE ?
                bne     ABORT2          ; NO...ABORT
                ldd     -2,X            ; GET NO. OF PARAMS
                beq     CS500           ; =O ? YES...SKIP
                cmpd    #4              ; =4 ?
                bne     ABORT2          ; NO...ABORT

CS500           pshs    U
                leau    COL0,U
                ldy     #COLMAP         ; COLOUR MAPPER BASE ADDRESS
                lda     #4              ; COUNTER
                sta     COUNT
CSLOOP2         ldd     ,X++            ; GET COLOUR REQUESTED
                andb    #$0F            ; MODULO 15 IN BITS 3-0
                stb     ,Y+             ; STORE IN COLOUR MAP
                stb     ,U+             ; AND IN STATIC STORAGE
                dec     COUNT           ; DECREMENT COUNTER
                bne     CSLOOP2
                puls    U
                clra                    ; ENSURE CARRY CLEAR
                rts
                opt     -l
                ttl     clear           ; graphics draw page
                opt     l
                pag
*
*GCLEAR - CLEARS CURRENT GRAPHICS PAGE
*       - (X) REG POINTS TO START OF PARAMETER BUFFER
*       - EXPECTS P1 INTEGER DEFAULT COLOUR 0
*
GCLEAR          clr     PMIN            ; VALUE OF EXPECTED P1
                lbsr    GET1D1          ; RETURNS VALID P1 IN (B) REG
                bcs     ABORT2          ; ERROR...ABORT
                tst     DFLAG           ; WAS DEFAULT VALUE USED ?
                beq     GC100
                decb                    ; DEFAULT COLOUR 0
GC100           cmpd    #MAXCOL         ; REQUESED COL >15 ?
                bhi     ABORT2          ; YES...ABORT
                stb     BGCOL           ; STORE BACKGROUND COLOUR NUMBER
                lbsr    GETCOL          ; GET COLOUR MASKS
                std     BCMSK1          ; STORE BACKGROUND LOWER BYTE & HIGHER BYTE
*
* FALL THROUGH TO CLEARPAG
*
                pag
*
*CLEARPAG    -STORES BACKGROUND COLOUR MASK IN GRAPHICS
*             MEMORY FOR CURRENT DRAW PAGE
*            -USES A DOUBLE BYTE CONTAINING SHIFTED
*             COLOUR MASKS TO CLEAR 4,8 OR 16 PIXELS
*             TO BACKGROUND COLOUR PER MEMORY ACCESS
*             DEPENDING ON RESOLUTION
*
CLEARPAG        ldd     CGPAGE          ; REL START OF PAGE PHYS MEM
                addd    MEMSTRT         ; ACTUAL START OF PAGE PHYS MEM
                std     PSTART

                ldb     #2              ; )CALCULATES 10240*R ie PAGE SIZE
                lbsr    BASADR          ; )
                subd    #1              ; NOW END OF PHYSICAL PAGE
                addd    PSTART          ; ADD ACTUAL START OF PHYSICAL PAGE
                std     PEND

                ldd     BCMSK1          ; LOWER BYTE BG COLOUR MASK
                bsr     BBYTE           ; PRODUCE LOWER DOUBLE BYTE MASK
                std     DBMSK1          ; STORE MASK

                leas    -4,S
                ldd     PSTART          ; GET PHYS ADDR OF
CPLOOP          cmpa    MYBLK           ; IS BLOCK ALREADY MAPPED?
                beq     CP15
                sta     NEWBLK
                lbsr    MAPBLOCK        ; NO...MAP IN BLOCK
                bcs     CP30
                lda     NEWBLK          ; UPDATE MAPPED IN BLOCK POINTER
                sta     MYBLK
CP15            ldd     PSTART          ; YES...CONTINUE
                addd    #TWOK
                std     PEND2K          ; GET END OF 4K BLOCK
                ldd     DBMSK1
                std     2,S
                ldd     PSTART
                anda    #BLKMAP
                ora     LBADDR
                std     ,S
                addd    #FOURK
                std     LEND4K
                bsr     CLEAR           ; CLEAR 4K BLOCK TO BACKGROUND COLOUR

                ldd     PEND2K          ; IS END OF 4K BLOCK=END OF PAGE
                cmpd    PEND
                beq     CP30            ; YES...SKIP
                addd    #1              ; NO..GET START OF NEXT 4K BLOCK
                std     PSTART
                bra     CPLOOP          ; AND REPEAT
CP30            leas    4,S             ; RESTORE STACK
                rts
                pag
*
*CLEAR   -CLEARS A 4K BLOCK ON CURRENT
*         GRAPHICS DRAW PAGE TO BACKGROUND
*         COLOUR SPECIFIED
*    IN - START ADDRESS OF LOGICAL BLOCK ON STACK
*       - DOUBLE BYTE COLOUR MASK ON STACK
*
CLEAR           ldx     2,S             ; POINT AT 1st BYTE IN 4K BLOCK
                ldy     #$0800          ; SET COUNTER TO 2048
                ldd     4,S             ; GET MASK
CRLOOP          std     ,X++
                leay    -1,Y
                bne     CRLOOP
                rts
                pag
*
*BBYTE    -FORMS DOUBLE BYTE COLOUR MASK USED IN
*          GCLEAR.
*    IN - (B) REG CONTAINS COLOUR MASK
*   OUT - (D) REG CONTAINS DOUBLE BYTE MASK
*
BBYTE           pshs    D               ; STACK COLOUR MASK
                ldx     #3
BBLOOP1         lsr     ,S              ; SHIFT AND ADD TO ORIGINAL MASK
                lsr     1,S             ; THREE TIMES
                addd    ,S 
                leax    -1,X
                bne     BBLOOP1
                tst     COL16
                bne     BB10            ; SKIP IF RESOLUTION=2
                ldx     #4
BBLOOP2         lsr     ,S              ; SHIFT AND ADD TO ORIGINAL MASK
                lsr     1,S             ; A FURTHER FOUR TIMES
                addd    ,S
                leax    -1,X
                bne     BBLOOP2
BB10            leas    2,S             ; RESTORE STACK
                rts
                opt     -l
                ttl     select          ; draw colour
                opt     l
                pag
*
*COLOUR - SELECT DRAW COLOUR
*       - (X) REG POINTS TO START OF PARAMETER BUFFER
*       - EXPECTS P1 INTEGER DEFAULT COLOUR 1
*
COLOUR          clr     PMIN            ; MIN VALUE OF EXPECTED P1
                lbsr    GET1D1          ; RETURNS VALID P1 IN (B) REG
                bcs     ABORT3          ; ERROR...ABORT
                cmpd    #MAXCOL         ; REQUESTED COLOUR >15 ?
                bhi     ABORT3          ; YES...ABORT
                stb     DRCOL           ; STORE DRAW COLOUR NUMBER
                bsr     GETCOL          ; GET COLOUR MASKS
                std     DCMSK1          ; STORE DRAW COLOUR MASKS
                rts
                opt     -l
                ttl     get             ; colour masks
                opt     l
                pag
*
*GETCOL  -SELECTS COLOUR MASKS FOR
*          REQUIRED COLOURSET/RESOLUTION/COLOUR
*       IN - (B) REG CONTAINS REQUESTED COLOUR NUMBER
*           IN RANGE 0-15. WRAPS AROUND IF < 16 COLOURS AVAIL.
*      OUT- (A) REG CONTAINS LOWER BYTE OF COLOUR MASK
*           (B) REG CONTAINS HIGHER BYTE OF COLOUR MASK
*      NB   (B) REG CLEARED IF HIGHER BYTE NOT REQ.
*
GETCOL          ldx     TABENT          ; COLOUR MASK TABLE POINTER
                lda     B,X             ; GET LOWER BYTE MASK
                tst     ONEBYTE         ; IS ONLY ONE BYTE USED FOR COLOUR DEFINITION
                beq     GC40            ; NO...SKIP
                clrb                    ; ENSURE CARRY CLEAR
                tfr     A,B             ; DUPLICATE COLOUR MASK IN (B)
                rts
GC40            lsrb                    ; REQD. COLOUR DIVIDE BY 2
                tst     COL16
                beq     GC50
                lsrb                    ; REQD. COLOUR DIVIDE BY 2
GC50            andcc   #$FE            ; ENSURE CARRY CLEAR
                ldb     B,X             ; GET HIGHER BYTE MASK
                rts
                opt     -l
                ttl     select          ; display page
                opt     l
                pag
*
* SUBROUTINE GRAPHICS
*
*         - SELECT GRAPHICS PAGE TO DISPLAY
*         - (X) REG POINTS TO START OF PARAMETER BUFFER
*          -EXPECTS P1 INTEGER DEFAULT 1
*

DISPG           ldd     CDPAGE          ; ENTRY POINT FOR TOGGLE
                bra     GR100           ; BETWEEN GRAPHICS AND TEXT SCREENS
GRAPHICS        lda     #1              ; MIN EXPECTED P1
                sta     PMIN
                lbsr    GET1D1          ; RETURNS VALID P1 IN (B) REG
                bcs     ABORT3          ; ERROR...ABORT
                cmpb    GPAGES          ; VALID PAGE ? **MODULO 256**
                bhi     ABORT3          ; NO...ABORT
                lbsr    BASADR          ; CALC. ADDRESS OF (0,Ymax)
                std     CDPAGE          ; STORE P1
GR100           lbsr    PIASET
                lbra    DISPAG
                opt     -L
                ttl     translate       ; coordinate origin
                opt     l
                pag
*
*TRANS - (X,Y) COORDINATE ORIGIN TRANSLATION
*          - (X) REG POINTS TO PARAMETER BUFFER
*          - EXPECTS P1, P2 INTEGER DEFAULT RESET TO (0,0)
*
TRANS           ldd     ,X++            ; GET NO.OF PARAMS
                beq     TR100           ; SKIP IF 0
                cmpd    #2              ; NO.OF PARAMS =2?
                bne     ABORT3          ; SKIP IF NOT
                ldd     XTRAN           ; ELSE GET CURRENT X TRANS
                addd    ,X++            ; ADD P1
                lbvs    RO200           ; OVERFLOW?
                std     XTRAN           ; STORE NEW X TRANS
                ldd     YTRAN           ; GET OLD Y TRANS
                addd    ,X              ; ADD P2
                lbvs    RO200           ; OVERFLOW?
                std     YTRAN           ; STORE CURRENT Y TRANS
                clra                    ; ENSURE CARRY CLEAR
                rts
ABORT3          lbra    ABORT

TR100           clra
                clrb
                std     XTRAN           ; CLEAR X TRANS
                std     YTRAN           ; Y TRANS
                rts
                opt     -l
                ttl     apply           ; X and Y scale factors
                opt     l
                pag
*
*
*SCALE - X AND Y SCALE FACTORS
*      - (X)REG POINTS TO PARAMETER BUFFER
*      - EXPECTS P1,P2 INTEGER
*      - DEFAULT RESET TO 256, 256 (SCALE x1)
*
SCALE           ldd     ,X++            ; GET NO OF PARAMS
                beq     SC100           ; SKIP IF 0
                cmpd    #2              ; NO OF PARAMS = 2?
                bne     ABORT3          ; SKIP IF NOT
                leas    -9,S
                ldd     XSCALE          ; GET CURRENT X SCALE
                std     6,S             ; PUT INTO WORKSPACE
                ldd     ,X++            ; GET P1
                beq     SC200           ; SKIP IF P1=0
                std     4,S
                lbsr    MULT            ; )16X16 SIGNED MULTIPLY RETURNS
                ldb     ,S              ; OUT OF RANGE ?
                bne     SC200           ; YES...ABORT
                ldd     1,S             ; GET RESULT/256
                bmi     SC200           ; OUT OF RANGE...SKIP
                tst     8,S
                beq     SC10            ; SKIP IF +VE
                coma
                comb
                addd    #1              ; ELSE 2'S COMP
SC10            std     2,X             ; TEMP STORE NEW X SCALE
                ldd     YSCALE          ; NOW SAME FOR Y SCALE
                std     6,S
                ldd     ,X++
                beq     SC200
                std     4,S
                lbsr    MULT
                ldb     ,S
                bne     SC200
                ldd     1,S
                bmi     SC200
                tst     8,S
                beq     SC20            ; SKIP IF +VE
                coma
                comb
                addd    #1              ; ELSE 2'S COMP
SC20            std     YSCALE
                ldd     ,X              ; STORE NEW X SCALE
                std     XSCALE

                lda     TRFLAG          ; )SET SCALE FLAG
                ora     #%00000001      ; )
                sta     TRFLAG          ; )
                leas    9,S             ; RESTORE STACK
                lbra    TRANSFM

SC100           ldd     #SCAL0
                std     XSCALE
                std     YSCALE
                lda     TRFLAG
                anda    #%11111110
                sta     TRFLAG
                lbra    TRANSFM

SC200           leas    9,S             ; RESTORE STACK
                bra     RO200
                opt     -l
                ttl     apply           ; rotation
                opt     l
                pag
*
* ROTATE - X,Y AXES ROTATION
*        - (X) REG POINTS TO PARAMETER BUFFER
*       - EXPECTS P1 INTEGER DEFAULT =RESET X HORIZ.
*
ROTATE          ldd     ,X++            ; GET NO. OF PARAMS
                beq     RO100           ; SKIP IF 0
                cmpb    #1              ; NO. OF PARAMS =1?
                lbne    ABORT           ; SKIP IF NOT
                ldd     ROTAT           ; GET CURRENT ROTATE
                addd    ,X              ; ADD P1
                bvs     RO200           ; OVERFLOW?
RO10            subd    #360            ; )GET IN RANGE
                bpl     RO10            ; ) 0 TO 360
RO20            addd    #360            ; )
                bmi     RO20            ; )
                std     ROTAT           ; STORE VALUE
                lbsr    SINX0           ; GET SINE OF ANGLE
                std     SINTH           ; x16384
                ldd     ROTAT
                addd    #90
                lbsr    SINX0           ; GET COSINE OF ANGLE
                std     COSTH           ; x16384
                lda     TRFLAG          ; )SET ROTATE FLAG
                ora     #%00000010      ; )
                sta     TRFLAG
                lbra    TRANSFM

RO100           ldd     #0
                std     ROTAT           ; CLEAR ROTATE
                std     SINTH           ; SINE=0x16384=0
                ldd     #COS0
                std     COSTH           ; COSINE=1x16384
                lda     TRFLAG          ; )CLEAR ROTATE FLAG
                anda    #%11111101      ; )
                sta     TRFLAG          ; )
                lbra    TRANSFM

RO200           inc     OVRFLW          ; SET OVERFLOW FLAG
                opt     -l
                ttl     move            ; cursor
                opt     l
                pag
*
*MOVE  - MOVES CURSOR TO POSITION (X,Y)
*      - (X) REG POINTS TO PARAMETER BUFFER
*      - EXPECTS P1,P2 INTEGER DEFAULT (0,0)
*
MOVE            ldd     ,X++            ; GET NO.OF PARAMS
                beq     MO100           ; SKIP IF 0
                cmpd    #2              ; NO.OF PARAMS =2?
                lbne    ABORT           ; SKIP IF NOT
                ldd     ,X++            ; GET P1
                std     CPOSX           ; STORE P1
                ldd     ,X              ; GET P2
                std     CPOSY           ; STORE P2
                rts

MO100           ldd     #0              ; SET CURSOR POSN.TO (0,0)
                std     CPOSX
                std     CPOSY
                rts
                opt     -l
                ttl     plot            ; line between two points
                opt     l
                pag
*
*PLOT - PLOTS LINE FROM CURSOR TO GIVEN (P1,P2)
*     - MOVES CURSOR TO (P1,P2)
*     - (X) REG POINTS TO PARAMETER BUFFER
*     - IF N=0 PLOTS POINT AT CURSOR POSITION
*
PLOT            ldd     ,X++            ; GET NO OF PARAMS
                bne     PL100           ; IS COUNTER <>0 - YES SKIP
                lda     TRFLAG          ; IS TRANSFORM FLAG SET?
                bne     PL010           ; YES....SKIP
                ldd     CPOSX           ; NO....GET CURSOR POSN AND
                addd    XTRAN           ; ADD X TRANSLATION FACTOR
                lbvs    PL400
                std     TX0             ; NO TRANSFORM APPLIED
                std     TX1
                ldd     CPOSY
                addd    YTRAN           ; ADD Y TRANSLATION FACTOR
                lbvs    PL400
                std     TY0
                std     TY1
                bra     PL300

PL010           ldd     CPOSX           ; GET CURSOR X
                std     XX              ; WORKING STORE
                ldd     CPOSY           ; GET CURSOR Y
                std     YY              ; WORKING STORE
                lbsr    XTRANS          ; APPLY X TRANSFORM
                bcs     PL400
                std     TX0
                std     TX1
                lbsr    YTRANS          ; APPLY Y TRANSFORM
                bcs     PL400
                std     TY0
                std     TY1
                bra     PL300

PL100           lda     TRFLAG          ; IS TRANSFORM FLAG SET?
                bne     PL200           ; YES....SKIP
                ldd     CPOSX           ; NO....GET CURSOR POSN
                addd    XTRAN           ; ADD X TRANSLATION FACTOR
                bvs     PL400
                std     TX0             ; NO TRANSFORM APPLIED
                ldd     CPOSY
                addd    YTRAN           ; ADD Y TRANSLATION FACTOR
                bvs     PL400
                std     TY0
                ldd     ,X++            ; GET P1 AND P2
                std     XX              ; TEMP STORE OF NEW CURSOR X
                addd    XTRAN           ; ADD X TRANSLATION FACTOR
                bvs     PL400
                std     TX1             ; NO TRANSFORM APPLIED
                ldd     ,X++
                std     YY              ; TEMP STORE OF NEW CURSOR Y
                addd    YTRAN           ; ADD Y TRANSLATION FACTOR
                bvs     PL400
                std     TY1
                bra     PL250

PL200           ldd     CPOSX           ; GET CURSOR
                std     XX
                ldd     CPOSY
                std     YY
                lbsr    XTRANS          ; APPLY X TRANSFORM
                bcs     PL400
                std     TX0
                lbsr    YTRANS          ; APPLY Y TRANSFORM
                bcs     PL400
                std     TY0
                ldd     ,X++            ; GET P1
                std     XX
                ldd     ,X++            ; GET P2
                std     YY
                lbsr    XTRANS          ; APPLY X TRANSFORM
                bcs     PL400
                std     TX1
                lbsr    YTRANS          ; APPLY Y TRANSFORM
                bcs     PL400
                std     TY1
PL250           ldd     ,--X
                std     CPOSY           ; NOW UPDATE CURSOR Y&X
                ldd     ,--X
                std     CPOSX

PL300           lbra    LPLOT           ; PLOT LINE
PL400           coma                    ; SET CARRY
                inc     OVRFLW          ; SET OVERFLOW FLAG
                rts
                opt     -l
                ttl     apply           ; matrix transformation
                opt     l
                pag
*
*TRANSFM - IF TRANSFORM FLAG SET - RECALCULATES
*            TRANSFORM MATRIX ELEMENTS
*          - IF TRANSFORM FLAG CLEAR - RESETS
*            TRANSFORM MATRIX ELEMENTS TO DEFAULTS
*
TRANSFM         lda     TRFLAG          ; IS TRANSFORM FLAG SET?
                bne     TF10            ; YES....SKIP
                ldd     #256            ; NO....SET DEFAULT VALUES
                std     MAT1            ; FOR MATRIX ELEMENTS
                std     MAT4
                ldd     #0
                std     MAT2
                std     MAT3
                rts

TF10            leax    BUFFER,U
                leas    -9,S
                ldd     XSCALE
                std     6,S             ; PUSH ONTO STACK
                ldd     COSTH
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 SIGNED MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     TF100           ; OVERFLOW ?
                std     ,X++            ; TEMP STORE RESULT
                ldd     XSCALE
                std     6,S
                ldd     SINTH
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     TF100           ; OVERFLOW ?
                std     ,X++            ; TEMP STORE RESULT

                ldd     YSCALE
                std     6,S             ; PUSH ONTO STACK
                ldd     SINTH
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 SIGNED MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     TF100           ; OVERFLOW ?
                std     ,X++            ; TEMP STORE RESULT

                ldd     YSCALE
                std     6,S
                ldd     COSTH
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     TF100           ; OVERFLOW ?
                std     MAT4            ; STORE RESULT
                ldd     ,--X            ; AND GET OTHER MATRIX
                std     MAT2            ; ELEMENTS FROM TEMP STORE
                ldd     ,--X
                std     MAT3
                ldd     ,--X
                std     MAT1
                leas    9,S             ; RESTORE STACK
                rts

TF100           inc     OVRFLW          ; INDICATE OVERFLOW ERROR
                leas    9,S
                lbra    ABORT
                opt     -l
                ttl     transform       ; X coordinate
                opt     l
                pag
*
*XTRANS - APPLIES TRANSFORMATION TO (XX,YY) TO
*         GET NEW X COORDINATE TX
*
XTRANS          leas    -9,S
                ldd     MAT2            ; GET MAT2
                bne     XTR20           ; SKIP IF <>0
                ldd     #0
                std     RES             ; ELSE INTERMEDIATE RESULT=0
                bra     XTR40

XTR20           std     6,S             ; PUSH ONTO STACK
                ldd     YY              ; GET Y COORD
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 MULTIPLY
                com     8,S             ; COMPLIMENT SIGN
                beq     XTR30
                bsr     B4SIGN
XTR30           std     RES             ; STORE INTERMEDIATE RESULT
                ldd     2,S
XTR40           std     RES+2
                ldd     MAT1            ; GET MAT1
                cmpd    #256            ; IS MAT1 =256 ie SCALE=1
                bne     XTR50           ; SKIP IF NOT
                ldd     XX              ; ELSE INTERMED.RESULT=XX
                std     1,S
                clr     ,S
                clr     3,S
                bra     XTR55

XTR50           ldd     MAT1            ; GET MAT1
                std     6,S             ; PUT ONTO STACK
                ldd     XX              ; GET XX
                std     4,S
                lbsr    MULT
                tst     8,S
                beq     XTR55
                bsr     B4SIGN
XTR55           ldd     2,S
                addd    RES+2
                std     2,S
                ldd     ,S
                bcc     XTR60
                addd    #1
XTR60           addd    RES
                std     ,S
                bmi     XTR70
                tsta
                bne     XTR100
                ldd     1,S
                bra     XTR80

XTR70           coma
                bne     XTR100
                ldd     1,S
                bpl     XTR100

XTR80           addd    XTRAN
                bvs     XTR100
                andcc   #$FE            ; ENSURE CARRY IS CLEAR
                leas    9,S             ; RESTORE STACK
                rts

XTR100          leas    9,S
                coma
                rts
                opt     -l
                ttl     2's             ; comp. of four byte number
                opt     l
                pag
*
*B4SIGN   ROUTINE TO FORM 2'S COMPLIMENT OF
*         4 BYTE RESULT OF 16x16 MULTIPLY
*
*  IN - 2,S TO 5,S CONTAIN 4 BYTE NUMBER (2,S MSB)
*       AND D ALSO CONTAINS 2,S
* OUT - D CONTAINS 2,S
*
B4SIGN          coma
                comb
                std     2,S
                ldd     4,S
                coma
                comb
                addd    #1
                std     4,S
                ldd     2,S
                bcc     B4.10
                addd    #1
                std     2,S
B4.10           rts
                opt     -l
                ttl     apply           ; Y transformation
                opt     l
                pag
*
*YTRANS - APPLIES TRANSFORMATION TO (XX,YY) TO
*         GET NEW Y COORDINATE TY
*
YTRANS          leas    -9,S
                ldd     MAT3            ; GET MAT3
                bne     YTR20           ; NO...SKIP
                ldd     #0              ; YES...INTERMED.RESULT=0
                std     RES
                bra     YTR40

YTR20           std     6,S             ; PUSH ONTO STACK
                ldd     XX
                std     4,S             ; PUSH ONTO STACK
                lbsr    MULT            ; 16X16 SIGNED MULTIPLY
                tst     8,S
                beq     YTR30
                bsr     B4SIGN

YTR30           std     RES             ; INTERMEDIATE RESULT
                ldd     2,S
YTR40           std     RES+2
                ldd     MAT4            ; GET MAT4
                cmpd    #256            ; IS MAT4=256 ? ie SCALE=1
                bne     YTR50           ; NO...SKIP
                ldd     YY              ; YES...RESULT=YY
                std     1,S
                clr     ,S
                clr     3,S
                bra     YTR55

YTR50           ldd     MAT4
                std     6,S             ; PUSH ONTO STACK
                ldd     YY
                std     4,S             ; PUSH ONTO STACK
                bsr     MULT            ; 16X16 SIGNED MULTIPLY
                tst     8,S
                beq     YTR55
                bsr     B4SIGN
YTR55           ldd     2,S
                addd    RES+2
                std     2,S
                ldd     ,S
                bcc     YTR60
                addd    #1

YTR60           addd    RES
                std     ,S
                bmi     YTR70
                tsta
                bne     YTR100
                ldd     1,S
                bra     YTR80

YTR70           coma
                bne     YTR100
                ldd     1,S
                bpl     YTR100

YTR80           addd    YTRAN
                bvs     YTR100
                andcc   #$FE            ; ENSURE CARRY IS CLEAR
                leas    9,S             ; RESTORE STACK
                rts
YTR100          leas    9,S
                coma
                rts
                opt     -l
                ttl     look            ; up sine of angle
                opt     l
                pag
*
*SINX0 - RETURNS SINE OF ANGLE IN 2'S COMPLIMENT
*        FORM
*INPUT - (D) REG: ANGLE X TYPE INTEGER
*OUTPUT - (D) REG: 16384*SIN(X) 2'S COMPLIMENT FORM
*
SINX0           leas    -1,S
                clr     ,S
SINX1           subd    #360
                bpl     SINX1
SINX2           addd    #360            ; )NOW 0<=X <360
                bmi     SINX2           ; )
                cmpd    #90             ; IS 0 <=X <=90
                bhi     GT90            ; NO...SKIP
                bra     SINX            ; YES...GET TABLE ENTRY
GT90            cmpd    #180            ; IS 90 <X <=180
                bhi     GT180           ; NO...SKIP
                coma                    ; YES...TABLE ENTRY =180-X
                comb                    ; SIGN +VE
                addd    #181
                bra     SINX            ; GET TABLE ENTRY
GT180           inc     ,S
                cmpd    #270            ; IS 180<X <=270
                bhi     GT270
                subd    #180            ; YES...TABLE ENTRY =X-180
                bra     SINX            ; GET TABLE ENTRY
GT270           coma                    ; 270<X <360
                comb                    ; TABLE ENTRY =360-X
                addd    #361

SINX            lsla                    ; 2 BYTES PER TABLE ENTRY
                rolb
                leay    SINETAB,PCR     ; POINT TO TABLE
                ldd     D,Y             ; GET TABLE ENTRY
                tst     ,S              ; IS SIGN FLAG SET
                beq     RET             ; NO...RETURN
                coma                    ; YES...2'S COMPLIMENT
                comb
                addd    #1
RET             leas    1,S
                rts
                opt     -l
                ttl     16              ; by 16 signed multiply
                opt     l
                pag
*
*MULT - A 16X16 BIT MULTIPLY ROUTINE
*     - AB*CD = A*C*65536+(B*C+A*D)*256+B*D
*
*INPUT ON STACK MULTIPLICAND 1 AT 8,S
*               MULTIPLICAND 2 AT 6,S
*
*OUTPUT 2,S TO 5,S 4 BYTE RESULT
*
*      HIGH    LOW
*
MULT            clr     10,S
                ldd     #0              ; )CLEAR 2 MSB'S OF
                std     2,S             ; )RESULT SPACE
                ldd     6,S             ; GET MULTIPLICAND 1
                bpl     MULT10          ; SKIP IF +VE
                coma                    ; ELSE GET ABSOLUTE VALUE
                comb
                addd    #1
                std     6,S
                com     10,S            ; TOGGLE SIGN FLAG
MULT10          ldd     8,S             ; GET MULTIPLICAND 2
                bpl     MULT20          ; SKIP IF +VE
                coma                    ; ELSE GET ABSOLUTE VALUE
                comb
                addd    #1
                std     8,S
                com     10,S            ; TOGGLE SIGN FLAG
MULT20          lda     9,S             ; GET B
                ldb     7,S             ; GET D
                mul
                std     4,S             ; B*D
                lda     8,S             ; GET A
                ldb     7,S             ; GET D
                mul
                addd    3,S
                std     3,S             ; ADD A*D*256
                bcc     MU100
                inc     2,S
MU100           lda     9,S             ; GET B
                ldb     6,S             ; GET C
                mul
                addd    3,S             ; ADD B*C*256
                std     3,S
                bcc     MU200
                inc     2,S
MU200           lda     8,S             ; GET A
                ldb     6,S             ; GET C
                mul
                addd    2,S
                std     2,S             ; ADD A*C*65536
                andcc   #$FE            ; ENSURE CARRY CLEAR FOR VALID RESULT
                rts
                opt     -l
                ttl     divide          ; routine
                opt     l
                pag
*
*DIVIDE - DIVIDE 4 BYTE STACK ENTRY BY
*         16384 AND ROUND RESULT TO
*         NEAREST WHOLE NUMBER - 2'S COMP
*         IF NEGATIVE
*INPUT    2,S TO 5,S 4 BYTE NUMBER
*         10,S SIGN FLAG 0=+VE
*
*OUTPUT   (D) REG REQD ANSWER IN 2'S COMP FORM
*
DIVIDE          lsl     4,S             ; ) GET BITS 29 TO 14 OF
                rol     3,S             ; ) 4 BYTE NUMBER IN (D)
                rol     2,S             ; ) REG
                lsl     4,S             ; )
                rol     3,S             ; )
                rol     2,S             ; )
                ldd     2,S             ; )
                tst     4,S             ; ROUND UP IF BIT 13 SET
                bpl     DI100
                addd    #1
DI100           tsta
                bmi     DI250

DI150           tst     10,S            ; IS ANSWER +VE?
                bpl     DI200           ; YES...SKIP
                coma                    ; NO...FORM 2'S COMP
                comb
                addd    #1
DI200           andcc   #$FE            ; ENSURE CARRY CLEAR FOR VALID RESULT
                rts

DI250           coma                    ; INDICATE ERROR
                rts
                opt     -l
                ttl     algorithm       ; to generate line points from end coordinates
                opt     l
                pag
*
*LPLOT  PLOTS LINE ON SCREEN TRANSFORMS HAVE BEEN APPLIED
*       (TX0,TY0) START COORDS OF LINE
*       (TX1,TY1) END COORDS OF LINE
*
LPLOT           lbsr    CLIP            ; CLIPPING ROUTINE
                ldb     PLFLAG          ; IS PLOT FLAG SET
                bne     LP100           ; YES...SKIP
                rts
LP100           clr     PLFLAG
                ldx     #1
                ldy     #1
                ldd     TX1             ; CALCULATE TX1-TX0
                subd    TX0
                bge     LP110           ; SKIP IF +VE
                coma                    ; ELSE 2'S COMPLIMENT TO
                comb                    ; GET ABSOLUTE VALUE
                addd    #1
                ldx     #-1
LP110           stx     SIGNDX          ; AND SIGN=-1
                std     DX              ; STORE ABSOLUTE VALUE
                ldd     TY1             ; CALCULATE TY1-TY0
                subd    TY0
                bge     LP120           ; SKIP IF +VE
                coma                    ; ELSE 2'S COMPLIMENT TO
                comb                    ; GET ABSOLUTE VALUE
                addd    #1
                ldy     #-1
LP120           sty     SIGNDY          ; AND SIGN = -1
                std     DY              ; STORE ABSOLUTE VALUE

                lbeq    LP400           ; SKIP IF DY=0 HORIZONTAL LINE
                ldd     DX
                lbeq    LP500           ; SKIP IF DX=0 VERTICAL LINE
                subd    DY
                lbeq    LP600           ; SKIP IF DX=DY 45 DEG LINE

*BRESENHAMS ALGORITM

*IF DX > DY
                bcs     LP300
                ldd     DY              ; ERR=2*DY-DX
                lslb
                rola
                subd    DX
                std     ERR
                subd    DX              ; INC1=2*(DY-DX)
                std     INC1
                ldd     DY              ; INC2=2*DY
                lslb
                rola
                std     INC2
LOOP1           lbsr    PPLOT           ; PLOT A POINT
                bcs     LP350
                ldd     ERR             ; ERROR FUNCTION +VE
                bmi     LP210           ; NO...SKIP
                addd    INC1            ; YES ERR...ERR=ERR+INC1
                std     ERR
                ldd     TY0             ; AND TY0=TY0+SIGNDY
                addd    SIGNDY
                std     TY0
                bra     LP220
LP210           addd    INC2            ; ELSE ERR=ERR+INC2
                std     ERR
LP220           ldd     TX0             ; IS TX0=TX1?
                cmpd    TX1
                bne     LP230
                rts                     ; YES...RETURN
LP230           addd    SIGNDX          ; NO...TX0=TX0+SIGNDX
                std     TX0
                bra     LOOP1           ; AND REPEAT

* IF DY > DX
LP300           ldd     DX              ; ERR=2*DX-DY
                lslb
                rola
                subd    DY
                std     ERR
                subd    DY              ; INC1=2*(DX-DY)
                std     INC1
                ldd     DX              ; INC2=2*DX
                lslb
                rola
                std     INC2
LOOP2           lbsr    PPLOT           ; PLOT A POINT
                bcs     LP350
                ldd     ERR             ; ERROR FUNCTION +VE
                bmi     LP310           ; NO...SKIP
                addd    INC1            ; YES...ERR=ERR+INC1
                std     ERR
                ldd     TX0             ; AND TX0=TX0+SIGNDX
                addd    SIGNDX
                std     TX0
                bra     LP320
LP310           addd    INC2            ; ELSE ERR=ERR+INC2
                std     ERR
LP320           ldd     TY0             ; IS TY0=TY1?
                cmpd    TY1
                bne     LP330
                rts                     ; YES...RETURN
LP330           addd    SIGNDY          ; NO...TY0=TY0+SIGNDY
                std     TY0
                bra     LOOP2           ; AND REPEAT

LP350           rts

*SPECIAL CASE HORIZONTAL LINE
LP400           lbsr    PPLOT           ; PLOT A POINT
                bcs     LP350
                ldd     TX0             ; IS TX0=TX1
                cmpd    TX1
                bne     LP410
                rts                     ; YES...RETURN
LP410           addd    SIGNDX          ; NO... TX0=TX0+SIGNDX
                std     TX0
                bra     LP400           ; AND REPEAT

*SPECIAL CASE VERTICAL LINE
LP500           lbsr    PPLOT           ; PLOT A POINT
                bcs     LP350
                ldd     TY0             ; IS TY0=TY1?
                cmpd    TY1
                bne     LP510
                rts                     ; YES...RETURN
LP510           addd    SIGNDY          ; NO...TY0=TY0+SIGNDY
                std     TY0
                bra     LP500           ; AND REPEAT

*SPECIAL CASE 45 DEGREE LINE
LP600           lbsr    PPLOT           ; PLOT A POINT
                bcs     LP350
                ldd     TX0             ; IS TX0=TX1?
                cmpd    TX1
                bne     LP610
                rts                     ; YES...RETURN
LP610           addd    SIGNDX          ; NO...TX0=TX0+SIGNDX
                std     TX0
                ldd     TY0
                addd    SIGNDY
                std     TY0             ; AND TY0=TY0+SIGNDY
                bra     LP600
                opt     -l
                ttl     part            ; of line clipping algorithm
                opt     l
                pag
*
* SUBROUTINE CLIP
*
*   (TX0,TY0) TO (TX1,TY1) CONTAIN END POINTS OF LINE TO BE PLOTTED.
*   THIS SUBROUTINE DOES CLIPPING IN THREE STAGES TO DETERMINE THE END
*   POINTS OF THE LINE WHICH ARE VISIBLE ON THE SCREEN.
*
*   FIRSTLY, A SIMPLE CHECK TO SEE IF BOTH END POINTS LIE WITHIN THE SCREEN
*   BOUNDARIES IN WHICH CASE THE LINE CAN BE PLOTTED WITH NO CLIPPING REQUIRED.
*   SECONDLY, A CHECK TO SEE IF THE LINE IS TOTALLY OUTSIDE OF THE SCREEN
*   BOUNDARIES IN WHICH CASE NO PLOTTING IS NECESSARY.
*
*
*   THIRDLY, IF THIS STAGE IS REACHE D THE LINE MUST INTERSECT THE SCREEN
*   BOUNDARIES, AND THE SUTHERLAND-COHEN MID POINT DIVISION CLIPPING ALGORITHM
*   IS USED TO FIND THE INTESECTIONS WITH THE SCREEN BOUNDARIES.
*
*   THE SUTHERLAND-COHEN ALGORITHM IS APPLIED TWICE - ONCE WITH (TX0,TY0) AS
*   POINT 0 AND (TX1,TY1) AS POINT 1, AND ONCE WITH THESE ASSIGNATIONS
*   REVERSRED
*   FOR LINE P0-P1, TO CALCULATE THE CLIPPED VALUE OF P1 THE FOLLOWING
*   STEPS ARE PERFORMED :-
*
*   (1) IS POINT P1 ON SCREEN ?
*            YES :- THEN CLIPPING COMPLETE
*             NO :- GO TO (2)
*
*   (2) CALCULATE PM, THE MID POINT OF P0-P1
*
*   (3) DOES ANY PART OF PM-P1 LIE ON SCREEN
*            YES :- THEN REPLACE P0 WITH PM
*             NO :- THEN REPLACE P1 WITH PM
*
*   (4) GO TO STEP 1
*
* INPUT  : TX0,TY0,TX1,TY1 CONTAIN THE END POINTS OF THE LINE
*
* OUTPUT : TX0,TY0,TX1,TY1 CONTAIN THE CLIPPED END POINTS OF THE LINE
*

CLIP            dec     PLFLAG          ; ASSUME LINE APPEARS ON SCREEN
                ldd     TX1             ; NOW SIMPLE CHECK - ARE ALL END
                cmpd    XMAX            ; COORDINATES WITHIN THE SCREEN
                bhi     CP50            ; BOUNDARIRES?
                ldd     TY1
                cmpd    YMAX            ; IF SO THEN LINE CAN BE PLOTTED
                bhi     CP50            ; WITH NO FURTHER CLIPPING.
                ldd     TX0
                cmpd    XMAX            ; IF NOT THEN GO TO MID POINT
                bhi     CP50            ; DIVISION CLIPPING
                ldd     TY0             ; ROUTINE
                cmpd    YMAX
                bhi     CP50
                clra                    ; ENSURE CARRY CLEAR
                rts

CP50            leas    -25,S           ; GET STACK FOR WORKSPACE
                leay    17,S            ; POINT TO END POINT WORKSPACES
                leax    9,S
                ldd     TX1             ; TRANSFER END POINTS INTO WORKSPACE
                std     6,Y
                ldd     TY1             ; WITH TX1,TY1 AS POINT 1
                std     4,Y
                ldd     TX0             ; AND TX0,TY0 AS POINT 0
                std     6,X
                ldd     TY0
                std     4,X

CP75            lda     #MAXITER
                sta     COUNT
CPLOOP1         exg     X,Y             ; USES (X) AS INDEX
                lbsr    TBLR            ; IS POINT 1 ON SCREEN
                exg     X,Y
                beq     CP200           ; YES...CLIPPING OF THIS END FINISHED
                lbsr    TBLR
                bsr     CF              ; IS P0-P1 ENTIRELY OFF SCREEN ?
                bne     CP400           ; YES...CLIPPING FINISHED
                lbsr    DIV2            ; GET MID POINT PM
                leax    1,S
                lbsr    TBLR
                bsr     CF              ; DOES ANY PART OF P1-PM APPEAR ON SCREEN ?
                bne     CP100           ; NO...SKIP

                leay    9,S
CP100           dec     COUNT           ; MAX ITERATIONS ?
                bpl     CP150           ; IF SO REPLACE P1 WITH P0
                leax    9,S
                leay    17,S
CP150           ldd     4,X             ; OVERWRITE COORDS OF
                std     4,Y
                ldd     6,X
                std     6,Y
                leay    17,S            ; RESTORE P1 AND P0 POINTERS
                leax    9,S
                bra     CPLOOP1         ; AND REPEAT FOR NEW P0-P1

CP200           tst     PLFLAG          ; -VE IF ONLY TX1,TY1 CLIPPED
                bpl     CP300           ; OR +VE IF BOTH ENDS CLIPPED...SKIP
                ldd     6,Y
                std     XX              ; TEMP STORE FOR CLIPPED (TX1,TY1)
                ldd     4,Y
                std     YY
                ldd     TX0             ; SET UP STACK WITH TX0,TY0 AS POINT 1
                std     6,Y             ; AND TX1,TY1 AS POINT 0
                ldd     TY0
                std     4,Y
                ldd     TX1
                std     6,X
                ldd     TY1
                std     4,X
                inc     PLFLAG          ; NOW GO AND CLIP POINT 0
                inc     PLFLAG
                bra     CP75

CP300           ldd     6,Y
                std     TX0             ; REPLACE (TX0,TY0) & (TX1,TY1) WITH CLIPPED VALUES
                ldd     4,Y
                std     TY0
                ldd     XX
                std     TX1
                ldd     YY
                std     TY1
                bra     CP500
CP400           clr     PLFLAG          ; LINE NOT VISIBLE
CP500           leas    25,S            ; RESTORE STACK
                clr     COUNT
                rts                     ; AND RETURN
                pag
*
* SUBROUTINE CF
*
*   CALCULATES CF=T0*T1+B0*B1+L0*L1+R0*R1 FOR LINE FROM POINT 0 TO POINT 1
*   THIS FUNCTION IS ZERO IF ANY PART OF THE LINE SEGMENT COULD APPEAR
*   ON THE SCREEN, AND HAS A VALUE OF 1 IF THE LINE LIES TOTALLY OUTSIDE
*   THE SCREEN BOUNDARIES.
*
* INPUT  : T,B,L,R, VALUES FOR POINTS 0 AND 1 ARE ON STACK, INDEXED BY
*          (Y) FOR POINT 1 AND (X) FOR POINT 0.
*
* OUTPUT : (B) CONTAINS CF
*
CF              lda     ,X
                ldb     ,Y
                mul                     ; ALL RESULTS OF MULTIPLICATIONS ARE
                stb     2,S             ; EITHER 0 OR 1 ie IN (B) ONLY
                lda     1,X
                ldb     1,Y
                mul
                addb    2,S
                stb     2,S
                lda     2,X
                ldb     2,Y
                mul
                addb    2,S
                stb     2,S
                lda     3,X
                ldb     3,Y
                mul
                addb    2,S
                rts
                pag
*
* SUBROUTINE DIV2
*
*   TO CALCULATE THE COORDINATES OF THE MID POINT OF LINE (TX0,TY0)-(TX1,TY1)
*   MID POINT =END POINT 0 +/- (END POINT 1 - END POINT 0)/2
*   THE END POINTS OF THE LINE ARE 2'S COMPLIMENT NUMBERS IN RANGE
*   -32767 TO +32767. THE DIFFERENCE BETWEEN THEM COULD BE UP TO 65534
*   WHICH IS OUT OF RANGE FOR A 2'S COMPLIMENT NUMBER, BUT HALF OF THE
*   DIFFERENCE (TRUNCATED, NOT ROUNDED) IS UP TO 32767, ie IN RANGE FOR
*   2'S COMPLIMENT AGAIN. THEREFORE THE SIGN AND MAGNITUDE OF HALF THE
*   DIFFERENCE ARE CALCULATED SEPARATELY, THEN THE VALUE IS SIGNED AND
*   ADDED TO END POINT 0
*
* INPUT  : END POINT COORDS. ON STACK INDEXED BY (Y) AND (X)
*
* OUTPUT : MID POINT COORDINATES ON STACK AT 12,S FOR X COORD. AND
*          10,S FOR Y COORD RESP.
*

DIV2            clr     2,S             ; ASSUME -VE
                ldd     6,Y             ; GET X COORD OF POINT 1
                subd    6,X             ; SUBTRACT X COORD OF POINT 0
                blt     DV100           ; IS ANSWER TO BE -VE ?YES...SKIP
                inc     2,S             ; NO...SET SIGN FLAG
                addd    #1              ; ROUND UP IF +VE
DV100           lsra                    ; DIVIDE BY 2 AND ADD
                rorb                    ; TO X COORD OF POINT 0
                addd    6,X             ; TO GIVE X COORD OF MID POINT
                tst     2,S             ; WAS DIFFERENCE +VE
                bne     DV200           ; YES...SKIP
                addd    #DIFF           ; NO...ADJUST
                cmpd    #$FFFF
                bne     DV200
                clra
                clrb
DV200           std     9,S

                clr     2,S
                ldd     4,Y             ; NOW SAME FOR Y COORDS.
                subd    4,X
                blt     DV250
                inc     2,S
                addd    #1              ; ROUND UP IF +VE
DV250           lsra
                rorb
                addd    4,X
                tst     2,S
                bne     DV350
                addd    #DIFF
                cmpd    #$FFFF
                bne     DV350
                clra
                clrb
DV350           std     7,S
                rts
                pag
*
* SUBROUTINE TBLR
*
*   CALCULATES T,B,L,R VALUES FOR POINT INDEXED BY (X) REGISTER
*   USED IN SUTHERLAND-COHEN ALGORITHM TO DETERMINE WHETHER POINT IS ON
*   SCREEN, AND LINE JOINING TWO POINTS IS ON SCREEN.
*
*   TBLR2 EFFECTIVELY REDUCES SCREEN SIZE FROM 0 TO Xmax, 0 TO Ymax (AS TBLR)
*   TO 1 TO Xmax-1, 1 TO Ymax-1, TO ENABLE MID-POINT CLIPPING TO RECOGNISE
*   SCREEN BOUNDARIES.
*
* INPUT  : X COORD OF POINT AT 6,X  Y COORD OF POINT AT 4,X
*
* OUTPUT : VALUES OF L,R,T,B AT 0,X 1,X 2,X 3,X RESPECTIVELY
*          T+B+L+R IN (B) REGISTER   [=0 ON SCREEN =1 NOT ON SCREEN]
*

TBLR            ldd     #$0101          ; SET T,B,L,R TO ONE
                std     ,X
                std     2,X
                ldd     6,X             ; GET X COORD
                blt     TBLR10          ; <0...SKIP
                dec     ,X              ; NO...SET L=0
                cmpd    XMAX            ; >X max?
                bgt     TBLR20          ; NO...SKIP
TBLR10          dec     1,X             ; YES...SET R=0
TBLR20          ldd     4,X             ; GET Y COORD
                blt     TBLR30          ; <0...SKIP
                dec     3,X             ; NO...SET T=0
                cmpd    YMAX            ; >Y max ?
                bgt     TBLR40          ; YES...SKIP
TBLR30          dec     2,X             ; YES...SET B=0
TBLR40          ldb     3,X
                addb    2,X
                addb    1,X
                addb    ,X
                rts
                opt     -l
                ttl     plot            ; visible point in screen memory
                opt     l
                pag
*
*PPLOT    -CONVERTS (X,Y) & PAGE NUMBER TO PHYSICAL
*          MEMORY ADDRESS AND 'PLOTS' POINT IN MEMORY
*     IN - TX0  TY0 CONTAIN COORDS OF POINT TO BE PLOTTED
PPLOT           ldb     PLTMOD          ; XOR PLOT MODE ?
                bne     PPLOT2          ; YES...SKIP
PPLOT1          lda     GETMSK
                sta     BLKMSK
                bsr     MEMSHFT
                bcs     PP200           ; ERROR...SKIP
                tstb
                bne     PP20            ; ANY SHIFTS ?..NO..SKIP
                ldd     DCMSK1
                std     DCMSK1S         ; DRAW COLOUR MASKS NOT SHIFTED
                bra     PP40

PP20            pshs    B               ; STACK SHIFT COUNTER
                lda     DCMSK1
PPLOOP1         lsra                    ; SHIFT LOWER BYTE TO RIGHT
                lsr     BLKMSK          ; AND REVERSE OF BLKMSK
                decb                    ; DECREMENT COUNTER
                bne     PPLOOP1
                sta     DCMSK1S
                ldb     ,S+             ; GET SHIFT COUNTER AND RESTORE STACK
                lda     DCMSK2
PPLOOP2         lsra                    ; SHIFT HIGHER BYTE TO RIGHT
                decb                    ; DECREMENT COUNTER
                bne     PPLOOP2
                sta     DCMSK2S

PP40            com     BLKMSK          ; FORM BLANK MASK
                clrb                    ; CLEAR CARRY
PP50            ldb     PLTMOD          ; TEST PLOTMODE *** ENTRY POINT FROM PAINT ***
                beq     PP100           ; =0 ...SKIP
                ldb     ,X              ; =1 THEN XOR EXISTING COLOUR
                eorb    DCMSK1S         ; WITH REQD. COLOUR
                stb     ,X
                tst     ONEBYTE         ; 640 X 512 2 COLOUR MODE ?
                bne     PP60            ; YES...SKIP
                ldb     ,Y
                eorb    DCMSK2S
                stb     ,Y
PP60            ldd     TX0
                std     LASTX           ; SAVE THIS AS LAST POINT PLOTTED
                ldd     TY0
                std     LASTY
                rts

PP100           ldb     ,X              ; =0 THEN REPLACE EXISTING COLOUR
                andb    BLKMSK          ; WITH REQD. COLOUR
                orb     DCMSK1S
                stb     ,X
                ldb     ,Y
                andb    BLKMSK
                orb     DCMSK2S
                stb     ,Y
PP200           rts

PPLOT2          ldd     TX0             ; IS POINT TO BE PLOTTED SAME AS LAST POINT PLOTTED
                cmpd    LASTX           ; IF SO DO NOT PLOT AGAIN (PMODE=2 ONLY*********)
                bne     PPLOT1
                ldd     TY0
                cmpd    LASTY
                bne     PPLOT1
                clra                    ; ENSURE CARRY CLEAR
                rts
                opt     -l
                ttl     convert         ; from X,Y coords. to memory address/shift count
                opt     l
                pag
*
* SUBROUTINE MEMSHFT
*
*   CONVERTS (X,Y) COORDINATE PAIR TO MEMORY ADDRESS AND REQUIRED SHIFT TO
*   LOCATE CORRECT BIT/S IN PHYSICAL MEMORY, AND LOADS REQUIRED 4K BLOCK INTO
*   LOGICAL MEMORY.
*
*    RESOL.  COLS  R   B
*    320x256   4   2   0     X,Y =X AND Y COORDINATES
*    320x256  16   2   1     INT=INTEGER PART OF
*    620x512   4   4   0
*    640x512   4   4   1       P=PAGE NUMBER
*
*        MEMORY ADDRESS =
*        [MEMSTRT+R*P*B*5120-320*B-7]-[(320*B-7)*INT(Y/8)]-[Y]+[8*INT((X*B)/(4*R))]
*
*                [1]           -           [2]           -  [3]  +   [4]
*
*       [1] CONSTANT FOR GIVEN MODE, CALCULATED IN 'GMODE' OR 'GPAGE'
*       [2] (320*B-8) =642 OR 312. THIS VALUE /4 IS STORED AS BYTFAC
*       [4] DONE USING SHIFT MASK AND SHIFTS ACCORDING TO SHIFT FLAG
*
*         SHIFT REQUIRED=[X] - [(8*R)/(2*B)*INT((X*B)/(4*R))]
*
*                           [5]
*
*       [5] THIS IS CALCULATED QUICKLY WITH JUST A SHIFT MASK
*
*
* INPUT  : TXO,TYO CONTAIN X,Y COORDINATE PAIR
*          MCONST, BYTFAC, SHMSK, SHFLAG, SHMSKX CONTAIN REQUIRED CONSTANTS
*          MASKS AND FLAGS
*
* OUTPUT : (X) AND (Y) REGISTERS CONTAIN LOGICAL MEMORY BYTE ADDRESSES
*          (B) REGISTER CONTAINS REQUIRED NUMBER OF SHIFTS
*
MEMSHFT         ldd     TY0             ; CALCULATE [2]
                lsra                    ; INT(Y/8) MAX VALUE=63
                rorb                    ; 8 BIT ONLY - IN (B) REG
                andb    #SHMSK2
                lda     BYTFAC          ; 632=158*4 OR 312 =78*4
                mul                     ; 8X8 MULTIPLY D NOW CONTAINS [2]
                addd    TY0             ; [2] + [3]
                pshs    D
                ldd     TX0             ; NOW CALCULATE [4] WITH SHIFTS
                andb    SHMSK
                tst     SHFLAG
                beq     ME20
                bpl     ME10
                lsra
                rorb
                bra     ME20
ME10            lslb
                rola
ME20            addd    MCONST          ; [1] + [4]
                subd    ,S++            ; [1] - [2] - [3] + [4]
                std     PSTART
                anda    #BLKNUM
                cmpa    MYBLK           ; IS BLOCK ALREADY MAPPED?
                beq     ME30
                sta     NEWBLK
                bsr     MAPBLOCK
                bcs     ME40
                lda     NEWBLK          ; UPDATE MAPPED IN BLOCK POINTER
                sta     MYBLK
ME30            ldd     PSTART
                anda    #BLKMAP
                lslb
                rola
                ora     LBADDR
                tfr     D,X             ; LOGIGAL ADDRESS OF LOWER BYTE
                addd    #1              ; 2ND BLOCK OFFSET ABOVE 1ST
                tfr     D,Y             ; LOGIGAL ADDRESS OF UPPER BYTE

                ldb     TX0+1           ; CALCULATE SHIFT REQUIRED
                andb    SHMSKX
                lda     ONEBYTE         ; ONLY ONE BYTE USED ? (640 X 512 2 COLOUR MODE)
                bne     ME50            ; YES...SKIP
ME40            rts

ME50            cmpb    #MAXSHFT        ; IF SHIFT REQ. >7 THEN USE ODD BYTE
                bhi     ME60
                tfr     X,Y             ; ELSE USE EVEN BYTE
                clra                    ; ENSURE CARRY CLEAR
                rts

ME60            tfr     Y,X
                andb    #MAXSHFT        ; TAKE SHIFT-8 AS SHIFT COUNTER FOR ODD BYTE
                rts
                opt     -l
                ttl     map             ; requested block into tasks memory map
                opt     l
*
*MAPBLOCK  MAPS REQUIRED PHYSICAL BLOCK INTO
*          LOGICAL MEMORY AFTER UNMAPPING
*          EXISTING LOGICAL BLOCK
*
*  IN  - MYBLK EXISTING BLOCK NO. (BITS 4-7)
*        NEWBLK,U AND (A) REQD. BLOCK (BITS 4-7 )
* OUT  - LBADDR1,U BASE ADDRESS OF LOWER LOGICAL BLOCK
*
MAPBLOCK        pshs    U
                ldu     >D.Proc
                pshs    U
                ldu     >D.SysPrc
                stu     >D.Proc
                bsr     CLRBLOCK        ; UNMAP EXISTING BLOCK/S
                bcs     MB200
                clra
                lsrb
                lsrb
                lsrb
                tfr     D,X
                ldb     #1              ; NO. OF BLOCKS=1
                os9     F$MapBlk        ; MAP IN REQD BLOCKS
                bcs     MB100
                stu     LBADDR
MB100           puls    U
                stu     >D.Proc
                puls    U
                bcc     MB200           ; ANY ERROR ?
                stb     ERROR           ; YES...SAVE SYSTEM ERROR CODE
MB200           rts
                opt     -l
                ttl     unmap           ; block from address space
                opt     l
                pag
*
*CLRBLOCK  - UNMAPS BLOCK/S FROM LOGICAL MEMORY
*
*  IN  - LBADDR1,U CONTAINS LOGICAL
*        START ADDRSES OF LOWER BLOCK
*
CLRBLOCK        pshs    A
                ldu     LBADDR          ; GET START OF BLOCK IN U
                beq     CB200           ; NO...SKIP
                ldb     #1              ; NO. OF BLOCKS=1
                os9     F$ClrBlk        ; UNMAP REQD BLOCKS
                bcs     CB300
                clr     MYBLK
                clra
                clrb
                std     LBADDR
CB200           puls    B
                rts
CB300           stb     ERROR           ; SAVE SYSTEM ERROR CODE
                rts
                opt     -l
                ttl     DRAW            ; routine
                opt     l
                pag
*
*DRAW - ROUTINE FOR LINE DRAWING
*       AND MOVEMENT RELATIVE TO CURSOR POSITION
*     - EXPECT CHARACTER/INTEGER PAIRS AS PARAMS
*     - (X) REG POINTS TO PARAMETER BUFFER
*
DRAW            lda     ,X+             ; GET CHARACTER
                cmpa    #'D             ; IS IT 'D'?
                bne     DR20            ; NO...SKIP
                sta     DCHAR           ; STORE CHARACTER
                bra     DM              ; YES...DRAW/MOVE ROUTINE
DR20            cmpa    #'M             ; IS IT 'M'?
                beq     DM              ; YES...DRAW/MOVE ROUTINE
*
* FALL THROUGH TO TT
*
                pag
*
*TT - ACTS ON INTEGER FROM DRAW STRING
*     T COMMAND
*
*   - (X) REG POINTS TO PARAMETER BUFFER
*
TT              ldd     ,X++            ; GET VALUE
                beq     TT30            ; =0 ?..YES...SKIP
                addd    HEAD            ; ADD CURRENT HEADING
                bvc     TT10            ; OVERFLOW?
                coma                    ; SET CARRY
                rts

TT10            subd    #360            ; )
                bpl     TT10            ; )GET IN RANGE 0 TO 360
TT20            addd    #360            ; )AND STORE AS NEW HEADING
                bmi     TT20            ; )
                std     HEAD            ; )

                lbsr    SINX0           ; CALCULATE SINE AND
                std     HSIN            ; COSINE OF NEW HEADING
                ldd     HEAD
                addd    #90             ; COS(X)=SIN(X+90)
                lbsr    SINX0
                std     HCOS
                rts

TT30            ldd     #0              ; RESET HEADING TO 0
                std     HEAD
                std     HSIN            ; SINE OF HEADING=0
                ldd     #COS0
                std     HCOS            ; COSINE OF HEADING=1(x16384)
                rts
                pag
*
*DM - ACTS ON INTEGER FROM DRAW STRING D OR M
*     COMMAND
*   - (X) REG POINTS TO PARAMETER BUFFER
*
DM              ldd     ,X++            ; GET INTEGER
                leas    -9,S            ; GET WORKSPACE ON STACK
                std     DMVAL           ; STORE VALUE
                std     6,S             ; PUSH INTO WORKSPACE
                ldd     HCOS
                std     4,S             ; PUSH INTO WORKSPACE
                lbsr    MULT            ; 16X16 MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     DM400
                addd    CPOSX           ; ADD CURRENT CURSOR X
                bvs     DM400
                std     XX              ; STORE - TEMPORARY

                ldd     DMVAL
                std     6,S
                ldd     HSIN
                std     4,S             ; PUSH INTO WORKSPACE
                lbsr    MULT            ; 16X16 MULTIPLY
                lbsr    DIVIDE          ; DIVIDE BY 16384
                bcs     DM400
                addd    CPOSY           ; ADD CURRENT CURSOR Y
                bvs     DM400
                std     YY              ; STORE - TEMPORARY
                leas    9,S             ; RESTORE STACK
                clra                    ; ENSURE CARRY CLEAR
                lda     DCHAR           ; DRAW OR MOVE ?
                bne     DM50            ; DRAW...SKIP
                ldd     XX              ; UPDATE CURSOR
                std     CPOSX
                ldd     YY
                std     CPOSY
                rts

DM50            clr     DCHAR
                leax    BUFFER,U        ; SET UP FOR PLOT
                ldd     #2
                std     ,X
                ldd     XX
                std     2,X             ; X AND Y COORD TO PLOT TO
                ldd     YY
                std     4,X
                lbra    PLOT            ; GO AND PLOT LINE

DM400           leas    9,S
                coma
                inc     OVRFLW          ; SET OVERFLOW FLAG
                rts
                opt     -l
                ttl     GGET/GPUT       ; functions
                opt     l
                pag
*
*GGET - ROUTINE TO COPY AREA OF SCREEN
*       TO ARRAY
*     - EACH BYTE OF ARRAY CONTAINS COLOUR
*       INFORMATION FOR 2 PIXELS (4 BITS EACH)
*     - EXPECTS P1, P2, P3 INTEGER
*       P1 WIDTH OF AREA TO GGET
*       P2 HEIGHT OF AREA TO GGET
*       P3 START ADDRESS OF ARRAY
*     - CURSOR POSITION IS BOTTOM LEFT CORNER
*       OF AREA TOGGET
*
*IN   - (X) POINTS TO START OF PARAMETER
*       BUFFER
*
GGET            leas    -19,S           ; WORKING AREA
                clr     ,S              ; CLEAR GPUT FLAG
                lbsr    PINIT           ; INITIALISATION ROUTINE
                bcc     GGLOOP
GG05            leas    19,S
                lbra    ABORT

GGLOOP          pshs    Y,X
                lbsr    GETPNT          ; GET COLOUR OF POINT IN B REG
                puls    Y,X
                bcs     GG05
                lda     ,S              ; IS BYTE HALF FULL?
                bne     GG10            ; YES...SKIP
                lslb                    ; NO...SHIFT TO BITS 7-4
                lslb
                lslb
                lslb
                stb     1,S             ; STORE HALF BYTE
                inc     ,S              ; SET HALF FULL FLAG
                bra     GG20

GG10            addb    1,S             ; YES...ADD TO 1ST HALF
                stb     ,X+             ; STORE BYTE AND INCREMENT POINTER
                clr     ,S              ; CLEAR HALF FULL FLAG
                lda     18,S            ; BUFFER FULL ?
                bne     GG15            ; NO...SKIP
                pshs    U,Y
                leax    GPBUFF,U        ; POINT TO START OF BUFFER
                ldy     >D.PROC         ; DESTINATION TASK NUMBER
                ldb     P$TASK,Y
                lda     >D.SysTsk       ; SOURCE TASK NUMBER
                ldy     #256            ; BUFFER SIZE
                ldu     11,S            ; SOURCE POINTER
                os9     F$Move          ; COPY BUFFER TO ARRAY
                puls    U,Y
                bcs     GG05            ; ERROR...ABORT
                lda     7,S
                adda    #$01            ; UPDATE ARRAY POINTER
                sta     7,S

GG15            dec     18,S            ; DEC BYTE COUNTER
GG20            lbsr    GEND            ; CHECK FOR END OF GGET
                lda     2,S             ; IS END FLAG SET?
                beq     GGLOOP          ; NO...REPEAT
                lda     ,S              ; YES..IS A HALF FULL BYTE NOT STORED
                beq     GG30            ; NO...SKIP
                ldb     1,S
                stb     ,X              ; YES..STORE HALF BYTE
                dec     18,S            ; DECR. BYTE COUNTER

GG30            ldy     #256            ; CALCULATE NUMBER OF BYTES REMAINING
                clra                    ; IN BUFFER TO BE TRANSFERRED
                ldb     18,S            ; TO ARRAY
                coma
                comb
                leay    D,Y
                pshs    U
                leax    GPBUFF,U        ; POINT TO START OF BUFFER
                pshs    X
                ldu     11,S            ; SOURCE POINTER
                ldx     >D.Proc
                ldb     P$Task,X        ; DESTINATION TASK NUMBER
                lda     >D.SysTsk       ; SOURCE TASK NUMBER
                puls    X               ; RESTORE BUFFER POINTER
                os9     F$Move          ; COPY REMAINING BYTES TO ARRAY
                puls    U
                lbcs    GG05
                leas    19,S            ; RESTORE STACK
                rts
                pag
*
*PINIT - CHECKS PARAMETER STRING PASSED
*      - REQUESTS SYSTEM MEMORY FOR TEMPORARY USE
*      - SETS POINTERS ON SCREEN AND IN
*        MEMORY
*      - EXPECTS P1, P2, P3 INTEGER
*IN    - (X) POINTS TO START OF PARAMETER BUFFER
*OUT   - (X) X POINTER (Y) MEMORY POINTER
PINIT           tst     2,S             ; GPUT ?
                bne     PI5             ; YES...SKIP
                ldd     CPOSX           ; CHECK THAT CURSOR IS ON SCREEN
                cmpd    XMAX
                lbhi    PI100
                ldd     CPOSY
                cmpd    YMAX
                bhi     PI100
PI5             ldd     ,X++            ; GET NUMBER OF PARAMETERS
                cmpd    #3              ; IS IT =3
                bne     PI100           ; NO...ABORT
                ldd     ,X++            ; IS P1 >0
                ble     PI100
                addd    CPOSX
                subd    #1
                tst     2,S             ; GPUT ?
                bne     PI10            ; YES...SKIP
                cmpd    XMAX            ; CHECK ALL OF GGET IS ON SCREEN
                bhi     PI100           ; IF NOT...ABORT
PI10            std     5,S             ; X max = P1 +CURSOR X -1

                ldd     ,X++            ; IS P2 >0
                ble     PI100
                addd    CPOSY
                subd    #1
                tst     2,S             ; GPUT ?
                bne     PI20            ; YES...SKIP
                cmpd    YMAX
                bhi     PI100
PI20            std     7,S             ; Y max = P2 + CURSOR Y -1
                ldd     ,X              ; STORE ARRAY POINTER IN (Y) REG
                std     9,S
                clr     4,S             ; CLEAR END FLAG
                ldd     CPOSY           ; INITIALISE X & Y POINTERS
                std     11,S            ; TO CURSOR X AND Y
                ldy     CPOSX
                tst     2,S             ; GGET ?
                beq     PI35            ; YES...SKIP
                leas    -9,S
                ldd     -2,X            ; CALC. NUMBER OF BYTES IN ARRAY
                std     6,S             ; =PARAM1*PARAM2/2 ROUNDED UP
                ldd     -4,X
                std     4,S
                lbsr    MULT
                ldd     2,S
                lsr     1,S
                rora
                rorb
                bcc     PI40
                addd    #1
PI40            std     22,S
                ldd     ,S
                leas    9,S
                bne     PI100           ; OVERFLOW ERROR...SKIP
                clr     20,S            ; SET BYTE COUNTER TO ZERO
                bra     PI30
PI35            ldd     TABENT          ; GET COLOUR MASK TABLE ENTRY POINTER
                std     13,S
                lda     GETMSK          ; GET COLOUR MASK 15
                sta     15,S            ; STORE AS 'GETMASK'
                ldb     #255            ; BUFFER COUNTER
                stb     20,S
PI30            clr     2,S             ; CLEAR HALF BYTE FLAG
                leax    GPBUFF,U        ; POINT TO START OF BUFFER
                rts
PI100           coma                    ; SET CARRY TO INDICATE ERROR
                rts
                pag
*
*GETPNT - GETS COLOUR OF POINT (X,Y)
*
*IN     - (X) X COORD OF POINT
*         11,S Y COORD OF POINT
*
*
*
*
GETPNT          sty     TX0             ; CALCULATE LOGICAL MEMORY
                ldd     15,S            ; ADDRESS AND SHIFT COUNTER
                std     TY0             ; FOR POINT (X,Y)
                lbsr    MEMSHFT         ; RETURNS LMEM1 LMEM2,COUNTER IN (B)
                bcs     GP40
                lda     ,X              ; GET MEMORY BYTE
                stb     20,S            ; STORE SHIFT COUNTER
GPLOP1          beq     GP10            ; SHIFT COUNTER =0? YES..SKIP
                lsla                    ; NO..SHIFT LEFT
                decb                    ; DECREMENT COUNTER
                bra     GPLOP1

GP10            anda    19,S            ; AND WITH GETMASK
                sta     21,S            ; STORE AS COLOUR BYTE 1
                ldb     ONEBYTE         ; 2ND BYTE REQUIRED
                bne     GP30            ; NO SKIP
                lda     ,Y              ; YES..GET 2ND BYTE AND
                ldb     20,S            ; COUNTER
GPLOP2          beq     GP20            ; SHIFT COUNTER=0? YES..SKIP
                lsla                    ; NO SHIFT LEFT
                decb                    ; DECREMENT COUNTER
                bra     GPLOP2

GP20            anda    19,S            ; AND WITH GETMASK
                sta     22,S            ; STORE AS COLOUR BYTE 2

GP30            bsr     COLGET          ; RETURNS COLOUR NUMBER IN (B)

GP40            rts
                pag
*
*GEND CHECK FOR END OF GGET OR GPUT
*
*IN (X) X POINTER (Y) MEMORY POINTER
*OTHER VARIABLES ON STACK
*
GEND            cmpy    5,S             ; MAX COLUMN REACHED?
                beq     GE10            ; YES..SKIP
                leay    1,Y             ; INCREMENT X COUNTER
                rts

GE10            ldy     CPOSX           ; RESET X POINTER
                ldd     11,S            ; INCREMENT Y POINTER
                addd    #1
                std     11,S
                cmpd    7,S             ; MAX ROW REACHED?
                bgt     GE20            ; YES..SKIP
                rts

GE20            inc     4,S             ; SET END FLAG
                rts
                pag
*
*COLGET RETURNS COLOUR NUMBER OF POINT
*
*
*
*
COLGET          ldx     19,S            ; GET TABLE POINTER
                clrb                    ; CLEAR COUNTER
                lda     23,S            ; GET COLOUR BYTE 1
CGLOOP1         cmpa    ,X              ; COMPARE WITH TABLE ENTRY
                beq     CG10            ; SAME...SKIP
                incb                    ; ELSE INCREMENT COUNTER
                leax    1,X             ; INCREMENT TABLE POINTER
                bra     CGLOOP1         ; AND REPEAT

CG10            lda     ONEBYTE         ; 2ND BYTE USED?
                beq     CG20            ; YES..SKIP
                rts                     ; NO...(B) CONTAINS COLOUR NUMBER

CG20            stb     23,S            ; STORE TABLE VALUE 1
                ldx     19,S            ; REPEAT FOR 2ND BYTE OF
                clrb                    ; COLOUR TO GET TABLE VALUE 2
                lda     24,S
CGLOOP2         cmpa    ,X
                beq     CG30
                incb
                leax    1,X
                bra     CGLOOP2

CG30            lslb                    ; MULT BY 2
                lda     COL16           ; 16 COLOUR MODE ?
                beq     CG40            ; NO...COL. NO. = 2*VALUE2 + VALUE1
                lslb                    ; YES...COL. NO. = 4*VALUE2 + VALUE1
CG40            addb    23,S
                rts
                pag
*
*GPUT - ROUTINE TO COPY ARRAY TO
*       SCREEN AND EXPAND ACCORDING TO
*       SCALE.
*     - EACH BYTE OF ARRAY CONTAINS COLOUR
*       INFORMATION OF 2 PIXELS
*     - EXPECTS P1, P2, P3, P4 INTEGER
*     - P1, P2, P3 AS GGET
*       P4 BACKGROUND COLOUR WHEN GGET DONE
*       (DEFAULT TO CURRENT SCREEN BACKGROUND COLOUR)
*
*       IN (X) POINTS TO START OF PARAMETER BUFFER
*
GPUT            leas    -19,S
                lda     #2
                sta     15,S            ; SET EXPANSION FLAG TO 2
                sta     ,S              ; SET GPUT FLAG
                ldd     XSCALE          ; GET X SCALE (X 256)
                bmi     GPT2            ; -VE X SCALE FACTOR ? ... THEN SKIP
                lbsr    SCFAC           ; GET X SCALE FACTOR
                bra     GPT3

GPT2            coma                    ; GET MODULUS OF SCALE FACTOR
                comb
                addd    #1
                lbsr    SCFAC           ; GET X SCALE FACTOR
                nega                    ; MAKE NEGATIVE
GPT3            sta     13,S

                ldd     YSCALE          ; NOW REPEAT FOR Y SCALE FACTOR
                bmi     GPT4
                lbsr    SCFAC
                bra     GPT5

GPT4            coma
                comb
                addd    #1
                lbsr    SCFAC
                nega
GPT5            sta     14,S

                ldd     ,X              ; IS NUMBER OF PARAMS =3?
                cmpd    #3
                beq     GPT10           ; YES...SKIP
                cmpd    #4              ; IS NUMBER OF PARAMS=4?
                lbne    GG05            ; NO...ABORT
                ldd     8,X             ; YES..GET P4
                cmpd    #15             ; P4>15?
                lbhi    GG05            ; YES..ABORT
                stb     NPCOL           ; NO..STORE AS COLOUR
                ldb     #3              ; NOT TO PLOT AND CHANGE
                std     ,X              ; NUMBER OF PARAMS TO 3
                bra     GPT20

GPT10           ldb     BGCOL           ; GET CURRENT BACKGROUND COL
                stb     NPCOL           ; STORE AS COLOUR NOT TO PLOT

GPT20           lbsr    PINIT           ; INITIALISATION ROUTINE
                lbcs    GG05            ; ERROR..ABORT

GPLOOP          pshs    U,Y
                leau    GPBUFF,U
                ldy     15,S            ; NO. OF BYTES TO READ
                ldd     15,S            ; REMAINING ARRAY SIZE
                tsta    IS              ; REMAINING ARRAY SIZE >256 ?
                beq     GPT22           ; NO...SKIP
                dec     15,S            ; REDUCE REMAINING ARRAY SIZE BY 256
                ldy     #256            ; NO. OF BYTES TO READ =256
GPT22           ldx     >D.PROC
                lda     P$TASK,X
                ldb     >D.SysTsk
                ldx     11,S            ; ARRAY POINTER
                inc     11,S            ; INC. ARRAY POINTER BY 256
GPT25           os9     F$Move
                tfr     U,X
                puls    U,Y
                bcs     GPT40
                dec     18,S            ; SET BYTE COUNTER TO 255

***
***   GPLOOP1 IS THE ENTRY POINT FROM PCHAR, THE ROUTINE CALLED BY
***     BY GPRINT TO PRINT TEXT CHARACTERS ON THE GRAPHICS SCREEN
***
GPLOOP1         ldb     ,X+             ; GET BYTE FROM BUFFER
                stb     1,S             ; STORE
                andb    #$F0            ; GET BITS 7-4
                lsrb
                lsrb
                lsrb
                lsrb                    ; SHIFT TO 3-0
                inc     ,S              ; SET HALF BYTE FLAG

GPLOOP2         pshs    Y,X
                bsr     PUTPNT          ; PUT POINT ON SCREEN
                puls    Y,X
                bcs     GPT40
                lbsr    GEND
                lda     2,S             ; IS END FLAG SET
                bne     GPT40           ; YES...SKIP
                lda     ,S              ; IS HALF BYTE FLAG SET?
                beq     GPT30           ; NO...GET NEXT BYTE
                ldb     1,S             ; YES...GET NEXT HALF BYTE
                andb    #$0F            ; BET BITS 3-0
                clr     ,S              ; CLEAR HALF BYTE FLAG
                bra     GPLOOP2

GPT30           lda     18,S
                beq     GPLOOP
                dec     18,S
                bra     GPLOOP1

GPT40           leas    19,S
                pshs    CC
                ldb     DRCOL
                lbsr    GETCOL
                std     DCMSK1          ; RESTORE DRAW COLOUR IN CASE IT HAS BEEN CHANGED BY GPUT
                puls    CC

                bcs     GPT.ERR         ; ANY ERROR YES...SKIP
                rts                     ; ELSE RETURN

GPT.ERR         tst     GPTFLAG         ; IS THIS A GPRINT ?
                lbeq    ABORT           ; NO...THEN ABORT
                inc     DMPFLAG         ; YES...THEN SET DUMP AND ABORT FLAGS
                inc     ABFLAG
                inc     PLFLAG          ; ALWAYS ASCII DUMP REQUIRED
                lda     #0
                sta     GPTFLAG         ; CLEAR GPRINT FLAG
                rts
                pag
*
* SUBROUTINE SCFAC
*
*   FOR SCALE FACTORS >1 ROUNDS TO NEAREST INTEGER
*   NOTE A SCALE FACTOR OF 1 IS REPRESENTED BY A SCALE VALUE OF 256
*
* INPUT  : (D) REG CONTAINS MAGNITUDE OF SCALE VALUE
*
* OUTPUT : (A) REG CONTAINS ROUNDED VALUE OF SCALE FACTOR
SCFAC           cmpd    #SCAL0
                bhi     SF10            ; SCALE FACTOR >1 ? ...THEN SKIP
                lda     #1              ; SET SCALE FACTOR TO 1
                clrb
                dec     17,S            ; DECREMENT EXPANSION FLAG
SF10            tstb
                bpl     SF20            ; ROUND UP IF NECESSARY
                inca
SF20            rts
                pag
*
*PUTPNT - PUTS ARRAY POINT ONTO SCREEN
*         AND EXPANDS ACCORDING TO SCALE
*         AFTER APPLYING TRANSFORMS
*    IN - (X) X COORD OF POINT BEFORE TRANSFORM
*       - 11,S Y COORD OF POINT BEFORE TRANSFORM
*       - (B) COLOUR OF POINT TO BE PLOTTED
*
PUTPNT          cmpb    NPCOL           ; IS IT A BACKGROUND POINT
                bne     PPT10
                rts                     ; YES..RETURN

PPT10           lbsr    GETCOL          ; GET COLOUR MASKS
                std     DCMSK1          ; STORE COLOUR MASKS

                lda     TRFLAG          ; IS TRANSFORM REQUIRED?
                bne     PPT20           ; YES...SKIP
                tfr     Y,D             ; NO...STORE X & Y FOR PPLOT
                addd    XTRAN           ; ADD X TRANSLATE FACTOR
                bvs     PPT60
                std     TX0
                ldd     15,S
                addd    YTRAN           ; ADD Y TRANSLATE FACTOR
                bvs     PPT60
                std     TY0
                bra     PPT30

PPT20           sty     XX              ; TRANSFORM POINT
                ldd     15,S            ; FOR PLOTTING
                std     YY
                lbsr    XTRANS
                bcs     PPT60
                std     TX0
                lbsr    YTRANS
                bcs     PPT60
                std     TY0

PPT30           ldd     TX0             ; IS POINT ON SCREEN
                cmpd    XMAX            ; -ONLY PLOT IF SO
                bhi     PPT40
                ldd     TY0
                cmpd    YMAX
                bhi     PPT40
                lbsr    PPLOT           ; PLOT POINT
                bcs     PPT50           ; SKIP IF ERROR
PPT40           lda     21,S            ; IS EXPANSION REQUIRED
                beq     PPT50           ; NO...RETURN
                bsr     EXPAND          ; YES..EXPAND POINT
PPT50           rts
PPT60           coma                    ; SET CARRY INDICATE ERROR
                inc     OVRFLW          ; OVERFLOW ERROR
                rts
                pag
*
*EXPAND - INCREASES SIZE OF POINT PLOTTED
*         BY GPUT ACCORDING TO X SCALE
*         AND YSCALE FACTOR, ROUNDED
*         TO NEAREST WHOLE NUMBER
*
*    IN - X SCALE AND Y SCALE FACTORS ON STACK
*         TX0 & TY0 BASE POINT TO EXPAND
*
EXPAND          ldd     TX0
                std     24,S
                lda     21,S            ; GET X COUNTER
                ldb     22,S            ; GET Y COUNTER
                ldx     TX0
                ldy     TY0
EXLOOP          cmpx    XMAX            ; IS POINT ON SCREEN
                bhi     EX10            ; NO...SKIP
                ldy     TY0
                cmpy    YMAX
                bhi     EX10            ; NO...SKIP
                pshs    A,B,X,Y
                lbsr    PPLOT           ; YES...PLOT POINT
                puls    A,B,X,Y
                bcc     EX10
                stb     ERROR           ; SYSTEM ERROR
                rts

EX10            tsta    NOW             ; CALCULATE NEXT POINT IN EXPANSION BLOCK
                bpl     EX20            ; IF X EXPANSION IN +VE DIRECTION THEN SKIP
                leax    -1,X
                stx     TX0             ; -VE EXPANSION
                inca                    ; INCREMENT X COUNTER (STARTS AT -(SCALE FAC) FOR -VE EXPANSION)
                bne     EXLOOP          ; MORE X EXPANSION ? REPEAT
                bra     EX30            ; NOW Y EXPANSION

EX20            leax    1,X
                stx     TX0             ; +VE EXPANSION
                deca                    ; DECREMENT X COUNTER (STARTS AT +(SCALE FACTOR) FOR +VE EXPANSION)
                bne     EXLOOP

EX30            ldx     24,S            ; RESET X POINTER
                stx     TX0
                lda     21,S            ; RESET X COUNTER

                tstb    NOW             ; APPLY SAME FOR +VE OR -VE Y EXPANSION
                bpl     EX40
                leay    -1,Y
                sty     TY0
                incb
                bne     EXLOOP
                rts                     ; EXPANSION BLOCK COMPLETE

EX40            leay    1,Y
                sty     TY0
                decb
                bne     EXLOOP
                rts                     ; EXPANSION BLOCK COMPLETE
                opt     -l
                ttl     paint           ; mode selection
                opt     l
                pag
*
* SUBROUTINE PMODE
*
*      - SELECTS PLOTTING MODE
*      - (X) REG POINTS TO PARAMETER BUFFER
*      - EXPECTS P1 INTEGER - DEFAULT 1
*

PMODE           lda     #1              ; GET MIN VALUE FOR P1
                sta     PMIN
                lbsr    GET1D1          ; GET P1, DEFAULT 1
                bcs     ABORT4
                cmpd    #2              ; P1>2?
                bhi     ABORT4          ; YES
                decb
                stb     PLTMOD          ; NO....STORE
                clra                    ; ENSURE CARRY CLEAR
                rts
                opt     -L
                ttl     PAINT           ; function
                opt     l
                pag
*
*PAINT SUBROUTINE TO FILL AREA AROUND CURSOR
*     -EXPECTS P1 INTEGER
*      PAINT COLOUR IS CURRENT DRAW COLOUR
*     -IF P1 GIVEN FILLS TO BORDER OF COLOUR P1
*      OR EDGE OF SCREEN
*
*     -IF P1 OMITTED FILLS TO ANY COLOUR OTHER
*      THAN THAT AT CURSOR
PAINT           lda     TRFLAG          ; GET CURRENT CURSOR POSITION
                bne     PT100           ; AND APPLY MATRIX TRANSFORMATION
                ldd     CPOSX           ; IF REQUIRED. IF NOT THEN JUST
                addd    XTRAN           ; ADD TANSLATION FACTORS.
                bvs     PT300
                std     TX0
                ldd     CPOSY
                addd    YTRAN
                bvs     PT300
                std     TY0
                bra     PT200

PT100           ldd     CPOSX
                std     XX
                ldd     CPOSY
                std     YY
                lbsr    XTRANS
                bcs     PT300
                std     TX0
                lbsr    YTRANS
                bcs     PT300
                std     TY0
PT200           clr     ABFLAG          ; CLEAR BORDER FLAG
                clr     PMIN            ; MIN VALUE=0
                lbsr    GET1D1          ; GET 1 PARAMETER-DEFAULT 1
                bcs     ABORT4          ; ERROR ABORT
                tst     DFLAG           ; WAS DEFAULT USED?
                bne     PT10            ; YES..SKIP
                cmpd    #MAXCOL         ; COLOUR NUMBER >15?
                bhi     ABORT4          ; YES..ABORT
                lbsr    GETCOL          ; GET COLOUR MASKS FOR
                bcs     ABORT4          ; ERROR ? YES...ABORT
                sta     DBMSK1          ; REQD COLOUR - LOWER BYTE
                stb     DBMSK2          ; AND HIGHER BYTE
                bra     PAINT1

ABORT4          lbra    ABORT

PT10            inc     ABFLAG          ; SET BORDER FLAG
                lbsr    MEMSHFT         ; GET LOGICAL ADDRESS AND SHIFT COUNTER
                lda     ,X              ; GET LOWER BYTE
                stb     COUNT           ; STORE SHIFT COUNTER
PTLOOP1         beq     PT20            ; SHIFT COUNTER=0 YES..SKIP
                lsla                    ; NO..SHIFT LEFT
                decb                    ; DECREMENT COUNTER
                bra     PTLOOP1
PT20            anda    GETMSK          ; AND WITH REQUIRED COLOUR 'GET MASK'
                sta     DBMSK1          ; STORE MASK - LOWER BYTE
                sta     DBMSK2          ; AND STORE AS HIGHER BYTE IN CASE OF ONEBYTE MODE
                lda     ONEBYTE         ; 2ND BYTE REQUIRED
                bne     PT50            ; NO...SKIP
                lda     ,Y              ; GET HIGHER BYTE
                ldb     COUNT           ; GET SHIFT COUNTER
PTLOOP2         beq     PT40            ; SHIFT COUNTER=0
                lsla                    ; NO SHIFT LEFT
                decb                    ; AND DECREMENT COUNTER
                bra     PTLOOP2
PT40            anda    GETMSK          ; AND WITH 'GET MASK'
                sta     DBMSK2          ; STORE AS COLOUR MASK-HIGH BYTE
PT50            bra     PAINT1

PT300           ldb     #ERROR2
                coma
                rts
                pag
*
*PAINT 1   INITIALISES PAINT ROUTINE
*          MAKES DUMMJY STACK ENTRY
*          AND MAKES INITIAL LEFT AND RIGHT
*          SCANS FOR FIRST TWO STACK ENTRIES
*
*IN        CPOSX CPOSY CURSOR POSITION
*OUT       ON STACK 2 ENTRIES X, Y COORD OF LEFT MUST BORDER
*          POINT, POINT COUNT FOR DIRECTIONS
*          UP AND DOWN
*

PAINT1          sts     STKSTRT         ; SAVE STACK POINTER ON ENTRY IN CASE OF
                ldb     #MAXSTCK        ; EXCESSIVE NO. OF STACK ENTRIES.
                stb     STKENTS         ; GET MAX NO. OF STACK ENTRIES (2's COMP)
                clra                    ; MAKE DUMMY STACK ENTRY WITH
                pshs    X,A             ; DIRECTION=0
                pshs    Y,D
                ldx     TX0             ; GET CURSOR X
                lbsr    SCANL           ; SCAN LEFT AND FILL UNTIL BORDER
                sty     INC1            ; STORE PART COUNT
                beq     PULSTACK        ; PART COUNT=0? YES PULL DUMMY
                lbsr    SCANCON         ; SCAN TO RIGHT
                ldb     #1
                stb     SIGNDY          ; DIRECTION FLAG UP
                lbsr    STCKENT         ; MAKE STACK ENTRY
                ldb     #-1
                stb     SIGNDY          ; DIRECTION FLAG DOWN
                ldy     #1              ; CONTINGENCY (Y)=0 CHECKED IN STCKENT
                lbsr    STCKENT
*
* FALL THROUGH TO PULSTACK
*
                pag
*
*PULSTACK    PULLS STACK ENTRY AND FILLS NEXT
*            LINE OR STOPS IF DUMMY ENTRY FOUND
*
*IN          ON STACK
*
*            Y COORD      2 BYTE ) OR LEFT...BORDER
*            DIRECTION FLAG 1 BYTE
*            X COORD      2 BYTE )
*            POINT COUNT  2 BYTE
*
*OUT         NONE
*

PULSTACK        dec     STKENTS         ; REDUCE STACK ENTRY COUNTER
                clra
                clrb
                sta     PNTFLAG
                std     RES
                puls    D,X
                leax    1,X             ; MOVE RIGHT-INTO FILL AREA
                stx     TX0             ; STORE VALUE
                std     CCOUNT          ; STORE PREVIOUS POINT COUNT
                puls    Y,A
                sta     SIGNDY          ; DIRECTION FLAG
                bne     PS10            ; =0 - DUMMY ENTRY?
                clr     COUNT           ; CLEAR FLAGS USED BY
                clr     ABFLAG          ; OTHER ROUTINES
                clr     ASIGN
                rts                     ; IF DUMMY ENTRY THEN FINISHED
PS10            bpl     PS30            ; IF DIRECTION IS UP THEN SKIP
                leay    -1,Y            ; DECREMENT Y POINTER
                cmpy    #0
                bpl     PS50            ; STILL ON SCREEN? YES..SKIP
                bra     PULSTACK        ; NO..GET NEXT ENTRY
PS30            leay    1,Y             ; INCREMENT Y POINTER
                cmpy    YMAX            ; STILL ON SCREEN
                bhi     PULSTACK        ; NO..GET NEXT ENTRY
PS50            sty     TY0             ; STORE Y POINTER
*
* FALL THROUGH TO SCAN
*
                pag
* SCAN THIS IS A COMPLICATED PIECE OF CODE
*      THE ONLY SATISFACTORY WAY TO SEE WHAT
*      IT IS DOING IS TO SIT DOWN AND WORK
*      THROUGH IT STARTING AT PAINT. IT TAKES
*      A LONG TIME BUT IS WELL WORTH THE EFFORT
*
*      IT CHECKS FOR FOUR POSSIBLE CASES OF
*      OVERHANG. THE SECTIONS WHICH ARE DOING
*      THE CHECKS ARE INDICATED. THE FOLLOWING
*      NOTES EXPLAIN THE CRITERIA FOR OVERHANG
*      *INDICATES BORDER PAINT
* 1)   REVERSE DIRECTION OVERHANG TO LEFT
*      *-------------* A PREVIOUS LEFT POINT
*     *            D C B
*    NEW LEFT POINT
*
*    LEFT COUNT STARTS WITH B IF COUNT >3 ie LEFT OF D THEN
*    REVERSE DIRECTION OVERHANG TO LEFT IS POSSIBLE AT ----
*
* 2) SAME DIRECTION OVERHANG TO LEFT -
*
*    *PREVIOUS LEFT POINT
*     *-------------* NEW LEFT POINT
*
*    IF SCANL PRODUCES ZERO COUNT POINT COUNT
*    IS DECREMENTED AND X IS INCREMENTED UNTIL NON
*    BORDER POINT IS FOUND
*
* 3) SAME DIRECTION OVERHANG TO RIGHT
*
*                                * PREVIOUS RIGHT POINT
*    NEW RIGHT *---------------*
*    POINT
*
*    MAX OVERHANG SIZE = POINT COUNT-RIGHT SCAN SIZE-1
*
*    NOTE 2) AND 3) SHARE SOME CODE WHICH IS VERY
*    CONFUSING!
*
* 4) REVERSE DIRECTION OVERHANG ON RIGHT
*
*           *   PREVIOUS X RIGHT
*             --------------* NEW RIGHT X
*
*    RIGHT SCAN SIZE - POINT COUNT -2 IS MAX SIZE
*    OF ANY OVERHANG IN REVERSE DIRECTION ON RIGHT
*
*    NOTE THESE ROUTINES COPE WITH MULTIPLE OVERHANG
*    E.G.
*                 *******   ****    ****
*    PREVIOUS     *     *   *   *  *    **
*    RIGHT X    **      *****   ***       *
*                                         * NEW RIGHT X
*
*
                pag
SCAN            lbsr    SCANL
                sty     INC1
                beq     SCN20           ; EAT UP BORDER FOR SAME DIRECTION LEFT OVERHANG
                cmpy    #3              ; REVERSE DIRECTION LEFT OVERHANG
                bcs     SCN10           ; NO..SKIP
                subd    #2
                std     XP              ; TEMP STORE FOR OVERHANG COUNT
                bsr     STCKENTR
SCN10           lbsr    SCANCON
                lda     ABFLAG
                bne     SCN15
                tst     PNTFLAG
                bne     SCN20
SCN15           bsr     STCKENT
SCN20           coma                    ; & CHECK FOR SAME DIRECTION RIGHT OVERHANG
                comb
                tfr     D,Y
SCN25           ldd     CCOUNT
                leay    D,Y
                sty     CCOUNT
                ble     SCN60
                ldx     TX0
                leax    1,X
                stx     TX0
                lbsr    TSTBDR
                beq     SCN50
                ldy     #-1
                bra     SCN25
SCN50           ldy     CCOUNT
                leax    -1,X
                stx     TX0
                stx     XX
                lbsr    SCANR
                std     YP              ; SAVE OVERHANG COUNT (=POINT COUNT)
                std     INC2
                bra     SCN15
SCN60           tfr     Y,D             ; CHECK FOR REVERSE DIRECTION
                coma                    ; OVERHANG TO RIGHT
                comb
                subd    #1
                ble     SCN70           ; NO OVERHANG...THEN PULL NEXT STACK ENTRY
                std     XP              ; SAVE OVERHANG COUNT
                ldx     TX0             ; CALCULATE LEFTMOST X AT START
                ldd     CCOUNT          ; OF OVERHANG
                leax    D,X
                leax    1,X
                stx     TX0
                bsr     STCKENTR
SCN70           lbra    PULSTACK
                pag
*
*STCKENTR MAKES STACK ENTRY WITH REVERSE
*         DIRECTION FOR REVERSE OVERHANGS
*

STCKENTR        inc     STKENTS         ; INCREMENT STACK ENTRY COUNTER
                beq     STKFUL          ; MAX NO. REACHED
                cmpy    #0              ; ANY COUNT
                beq     ST20            ; NO..THEN DON'T MAKE ENTRY
                puls    Y               ; PULL RETURN ADDRESS
                ldx     TY0             ; Y POINTER
                lda     SIGNDY          ; SIGN FLAG
                nega                    ; REVERSE DIRECTION
                pshs    X,A
                ldx     TX0             ; X POINTER-RIGHTMOST X
                ldd     XP              ; POINT COUNT
                pshs    X,D
                pshs    Y               ; RESTORE STACK
ST20            rts
                spc     5
*
*STCKENT MAKES STACK ENTRY FOR SAME DIRECTION
*        OVERHANG OR NORMAL NEXT LINE
*

STCKENT         inc     STKENTS         ; INCREMENT STACK ENTRY POINTER
                beq     STKFUL          ; MAX. NO. OF ENTRIES REACHED
                cmpy    #0              ; ANY POINT COUNT
                beq     ST10            ; NO..THEN DON'T MAKE ENTRY
                puls    Y               ; GET RETURN ADDRESS OFF STACK
                ldx     TY0             ; Y POINTER
                lda     SIGNDY          ; DIRECTION FLAG
                pshs    X,A
                ldx     XX              ; SECONDARY X POINTER-LEFT MOST X
                ldd     YP              ; POINT COUNT
                pshs    X,D
                pshs    Y               ; RESTORE RETURN ADDRESS
                ldd     INC2
ST10            rts
                spc     5
*
* SUBROUTINE STKFUL
*
*   IF MAX. NUMBER OF PAINT ROUTINE STACK ENTRIES HAVE BEEN MADE
*   THEN THIS RESTORES STACK POINTER TO BOTTOM OF PAINT ROUTINE STACK
*   AND RETURNS WITH APPROPRIATE ERROR CODE.
*

STKFUL          lds     STKSTRT         ; RESTORE STACK POINTER
                ldb     #ERROR3         ; STACK OVERFLOW ERROR CODE
                clr     COUNT
                clr     ABFLAG
                clr     ASIGN           ; CLEAR FLAGS FOR NEXT COMMAND
                coma                    ; SET CARRY TO INDICATE ERROR
                rts
                pag
*
*SCANCON   - NOW CONTINUE SCANNING TO RIGHT AND
*            GET TOTAL POINT COUNT
*
SCANCON         ldx     XX
                ldd     TX0
                std     XX
                stx     TX0
                bsr     SCANR
                leay    1,Y
                sty     INC2
                ldd     INC1
                leay    D,Y
                leay    -1,Y
                sty     YP
                tst     ABFLAG
                bne     SCC10
                ldd     RES
                coma
                comb
                addd    #1
                leay    D,Y
                bne     SCC10
                inc     PNTFLAG
                ldd     INC2            ; (D) SAME AS EXIT FROM STCKENT
SCC10           rts
                pag
*
*SCANL AND SCANR THESE ARE THE ACTUAL SCAN RIGHT/
*                LEFT, CHECK FOR BORDER, COUNT AND
*                FILL ROUTINES
*
*
SCANL           stx     XX              ; STORE AS SECONDARY X POINTER
                ldb     #-1
                stb     ASIGN           ; SET DECREMENT FLAG
                bra     SN10
SCANR           ldb     #1
                stb     ASIGN           ; SET INCREMENT FLAG
                ldx     TX0
                leax    1,X             ; INCREMENT PRIMARY X POINTER
SN10            ldy     #0              ; CLEAR POINT COUNTER
SCANLOOP        stx     TX0             ; STORE FOR PPLOT
                cmpx    XMAX            ; IS POINTER IN RANGE 0 - X max
                bhi     SN60            ; NO...SKIP
                bsr     TSTBDR          ; TEST FOR BORDER POINT
                bne     SN60            ; YES...SKIP
                pshs    X,Y
                ldx     LMEM1
                ldy     LMEM2
                lda     BYTES
                lbsr    PP50            ; NO..PAINT POINT (IN PPLOT)
                puls    X,Y
                leay    1,Y             ; INCREMENT POINT COUNT
                tst     ASIGN           ; INCREMENT?
                bpl     SN40            ; YES..SKIP
                leax    -1,X            ; DECREMENT X POINTER
                bra     SCANLOOP        ; AND REPEAT
SN40            leax    1,X             ; INCREMENT X POINTER
                bra     SCANLOOP        ; AND REPEAT
SN60            tfr     Y,D
                rts
                pag
*
*TSTBDR  CHECKS TO SEE IF POINT (TX0,TY0)
*        IS A BORDER POINT
*
TSTBDR          pshs    X,Y
                lbsr    MEMSHFT         ; GET LOGICAL BYTE CONTAINING POINT
                stx     LMEM1
                sty     LMEM2
                puls    X,Y
                lda     DCMSK1          ; GET DRAW COLOUR MASK LOW BYTE
                stb     COUNT           ; STORE SHIFT COUNTER
TBLOOP1         beq     TTB10
                lsra                    ; APPLY SHIFTS
                decb
                bra     TBLOOP1
TTB10           sta     DCMSK1S         ; STORE SHIFTED MASK
                lda     DBMSK1          ; GET BORDER MASK LOW BYTE
                ldb     COUNT           ; GET SHIFT COUNTER
TBLOOP2         beq     TTB20
                lsra                    ; APPLY SHIFTS
                decb
                bra     TBLOOP2
TTB20           sta     DBMSK1S         ; STORE SHIFTED MASK
                lda     GETMSK          ; GET 'GETMASK'
                ldb     COUNT           ; GETR SHIFT COUNTER
TBLOOP3         beq     TTB30
                lsra                    ; APPLY SHIFTS
                decb
                bra     TBLOOP3
TTB30           sta     GETMSKS         ; STORE SHIFTED MASK
                coma                    ; 1'S COMP. TO FORM BLANK MASK
                sta     BLKMSK          ; STORE SHIFTED MASK
                lda     DCMSK2          ; GET DRAW COLOUR MASK HIGH BYTE
                ldb     COUNT           ; GET SHIFT COUNTER
TBLOOP4         beq     TTB40
                lsra                    ; APPLY SHIFTS
                decb
                bra     TBLOOP4
TTB40           sta     DCMSK2S         ; STORE SHIFTED MASK
                lda     DBMSK2          ; GET BORDER MASK HIGH BYTE
                ldb     COUNT           ; GET SHIFT COUNTER
TBLOOP5         beq     TTB50
                lsra                    ; APPLY SHIFTS
                decb
                bra     TBLOOP5
TTB50           sta     DBMSK2S         ; STORE SHIFTED MASK

                lda     ABFLAG          ; IS BORDER FLAG SET?
                beq     TSTBDR2         ; SKIP OR FALL THROUGH
                pag
*
*TSTBDR1   CHECKS FOR BORDER AS ANY COLOUR
*          OTHER THAN THAT IN BORDER MASK BYTE/S
*          SETS (B) TO 0 IF NON-BORDER
*            OR (B) TO 1 IF BORDER
*
                lda     [LMEM1,U]       ; GET LOWER BYTE
                anda    GETMSKS         ; GET COLOUR OF POINT
                cmpa    DBMSK1S         ; IS IT BORDER COLOUR ?
                bne     TB200           ; NO...SKIP

                lda     [LMEM2,U]       ; GET HIGHER BYTE
                anda    GETMSKS         ; GET COLOUR OF POINT
                cmpa    DBMSK2S         ; IS IT BORDER ?
                bne     TB200           ; NO...SKIP

                clrb                    ; CLEAR BORDER FLAG
                rts

TB200           ldb     #1              ; SET BORDER FLAG
                rts
                pag
*
*TSTBDR2   CHECKS FOR BORDER AS COLOUR IN
*          BORDER MASK BYTE/S
*          SETS (B) TO 0 IF NON BORDER
*          SETS (B) TO 1 IF BORDER
*
TSTBDR2         lda     [LMEM1,U]
                anda    GETMSKS
                cmpa    DCMSK1S
                bne     TB210
                lda     [LMEM2,U]
                anda    GETMSKS
                cmpa    DCMSK2S
                bne     TB210

                ldd     RES             ; INCREMENT COUNTER OF POINTS PAINTED
                addd    #1
                std     RES
TB210           lda     [LMEM1,U]       ; GET LOWER BYTE
                anda    GETMSKS         ; GET COLOUR OF POINT
                cmpa    DBMSK1S         ; IS IT BORDER ?
                bne     TB300           ; NO...SKIP

                lda     [LMEM2,U]       ; GET HIGH BYTE
                anda    GETMSKS         ; GET COLOUR OF POINT
                cmpa    DBMSK2S         ; IS IT BORDER NO...SKIP
                bne     TB300           ; NO...SKIP
                ldb     #1              ; SET BORDER FLAG
                rts

TB300           clrb                    ; CLEAR BORDER FLAG
                rts
                opt     -l
                ttl     circle          ; drawing routine
                opt     l
                pag
*
*CIRCLE - DRAWS CIRCLE CENTRE AT CURRENT CURSOR
*       - (X) REG POINTS TO PARAMETER BUFFER
*       - EXPECTS P1 RADIUS >0
*                 P2 START ANGLE OF ARC DEFAULT 0
*                 P3 END ANGLE OF ARC DEFAULT 360
*
CIRCLE          ldd     ,X++            ; NO OF PARAMS < 0?
                beq     CR.ERR
                cmpd    #3
                bhi     CR.ERR
                ldy     ,X++            ; ELSE GET P1
                cmpy    #0              ; P1>0?
                ble     CR.ERR          ; SKIP IF NOT
                sty     RADIUS          ; ELSE STORE VALUE
                cmpb    #1              ; NO OF PARAMS =1?
                beq     CR100           ; SKIP IF SO
                ldy     ,X++            ; GET P2
                cmpy    #360            ; 0<=P2 <=360?
                bgt     CR.ERR          ; SKIP IF NOT
                cmpy    #-360
                blt     CR.ERR
                sty     TH1             ; ELSE STORE VALUE
                cmpb    #2              ; NO OF PARAMS=2?
                beq     CR200           ; SKIP IF SO
                ldy     ,X              ; GET P3
                cmpy    #360            ; 0<=P3<=360?
                bgt     CR.ERR          ; SKIP IF NOT
                cmpy    #-360
                blt     CR.ERR
                sty     TH2             ; STORE VALUE
                bra     DRAWCIR         ; GO AND DRAW CIRCLE
CR.ERR          lbra    ABORT

CR100           ldy     #0              ; SAT P2=0 DEFAULT
                sty     TH1
CR200           ldy     #360            ; SET P3=360 DEFAULT
                sty     TH2
*
* FALL THROUGH TO DRAWCIR
*
                pag
*
*DRAWCIR - CIRCLE DRAWING ROUTINE - CALCULATE
*          COORDS OF POINTS ON CIRCLE AND
*          PLOTS IT AS A NUMBER OF LINE
*          SEGMENTS
*          (X) REG POINTS TO PARAMETER BUFFER - USED BY DRAWCIR
*
DRAWCIR         lbsr    CINIT
                bcs     DC300
                leas    -6,S            ; GET STACK FOR CIRCLE PLOTTING
                ldd     CCOUNT          ; IS COUNTER =0
                beq     DC100           ; YES...SKIP
DCLOOP          bsr     RECALC          ; CALCULATE NEXT X,Y AND STORE IN BUFFER
                bcs     DC300
                pshs    X
                lbsr    PLOT            ; DRAW LINE SEGMENT
                puls    X
                bcs     DC300
                ldd     CCOUNT          ; IS COUNTER =0
                subd    #1
                std     CCOUNT          ; NO...DECREMENT COUNTER
                bne     DCLOOP          ; AND REPEAT LOOP
DC100           ldd     XEND            ; ELSE GET END (X,Y) OF ARC
                std     2,X             ; AND STORE IN BUFFER
                ldd     YEND
                std     4,X
                pshs    X
                lbsr    PLOT            ; NOW PLOT LAST ARC
                puls    X
DC300           leas    6,S             ; RESTORE STACK
                pshs    CC
                ldd     XCENT           ; MOVE CURSOR TO CIRCLE
                std     CPOSX           ; CENTRE AND RETURN
                ldd     YCENT
                std     CPOSY
                puls    CC
                bcs     CR.ERR          ; ABORT IF ERROR
                rts                     ; OR RETURN

                pag
*
*RECALC - CALCULATE NEXT POINT ON CIRCLE
*         FOR ANGULAR INCREMENT OF DTH
*         IF XP AND YP ARE COORDS OF POINT W.R.T.
*         (0,0) THEN
*         XP'=XP*COS (DTH)-YP*SIN(DTH)
*         YP'=XP*SIN (DTH)+YP*COS(DTH)
*         ADD XCENT TO XP,YCENT TO YP AND
*         STORE RESULTS IN BUFFER FOR PLOT ROUTINE
*         (X) REG POINTING AT START OF BUFFER
*
RECALC          ldd     XP
                std     5,S
                lda     XP+2
                sta     7,S
                ldd     YP
                std     2,S
                lda     YP+2
                sta     4,S
                lda     SHFT1
                sta     SHFCNT
SHIFT1          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT1          ; NO...REPEAT
                ldd     6,S
                addd    3,S
                std     6,S
                lda     5,S
                adca    2,S
                sta     5,S

                ldd     XP
                std     2,S
                lda     XP+2
                sta     4,S
                lda     SHFT2
                sta     SHFCNT
SHIFT2          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT2          ; NO...REPEAT
                ldd     6,S
                subd    3,S
                std     6,S
                lda     5,S
                sbca    2,S
                sta     5,S

                ldd     YP
                std     2,S
                lda     YP+2
                sta     4,S
                lda     SHFT0
                sta     SHFCNT
SHIFT3          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT3          ; NO...REPEAT
                ldd     6,S
                subd    3,S
                std     6,S
                lda     5,S
                sbca    2,S
                sta     5,S

                lda     7,S
                sta     XX+2
                ldd     5,S
                std     XX
                tst     7,S
                bpl     RE100
                addd    #1
RE100           addd    XCENT
                lbvs    RE400
                std     2,X

                ldd     YP
                std     5,S
                lda     YP+2
                sta     7,S
                ldd     XP
                std     2,S
                lda     XP+2
                sta     4,S
                lda     SHFT0
                sta     SHFCNT
SHIFT4          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT4          ; NO...REPEAT
                ldd     6,S
                addd    3,S
                std     6,S
                lda     5,S
                adca    2,S
                sta     5,S

                ldd     XP
                std     2,S
                lda     XP+2
                sta     4,S
                lda     SHFT1
                sta     SHFCNT
SHIFT5          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT5          ; NO...REPEAT
                ldd     6,S
                subd    3,S
                std     6,S
                lda     5,S
                sbca    2,S
                sta     5,S

                ldd     YP
                std     2,S
                lda     YP+2
                sta     4,S
                lda     SHFT2
                sta     SHFCNT
SHIFT6          asr     2,S
                ror     3,S
                ror     4,S
                dec     SHFCNT          ; SHIFT COUNTER=0 ?
                bne     SHIFT6          ; NO...REPEAT
                ldd     6,S
                subd    3,S
                std     6,S
                lda     5,S
                sbca    2,S
                sta     5,S

                lda     7,S
                sta     YP+2
                ldd     5,S
                std     YP
                tst     7,S
                bpl     RE200
                addd    #1
RE200           addd    YCENT
                bvs     RE400
                std     4,X
                clra
                ldd     XX
                std     XP
                lda     XX+2
                sta     XP+2
                rts

RE400           coma
                inc     OVRFLW
                rts
                pag
*
*CINIT INITIALISATION OF PARAMETERS FOR
*      CIRCLE/ARC DRAWING
*      (X) REG POINTS TO PARAMETER BUFFER
*
CINIT           leas    -9,S
                leax    BUFFER,U        ; POINT TO START OF PARAMETER BUFFER
                ldd     #2              ; STORE 2 AS NO. OF PARAMS.
                std     ,X

                ldd     CPOSX           ; GET CURSOR POSITION
                std     XCENT           ; AND STORE AS CIRCLE/ARC
                ldd     CPOSY           ; CENTRE
                std     YCENT

                ldd     RADIUS
                std     4,S
                ldd     TH2             ; CALCULATE END OF ARC
                lbsr    SINX0           ; XEND = XCENT+RADIUS*COS(TH2)
                std     6,S             ; YEND = YCENT+RADIUS*SIN(TH2)
                lbsr    MULT
                lbsr    DIVIDE
                lbcs    CS400
                addd    YCENT
                lbvs    CS400
                std     YEND
                ldd     TH2
                addd    #90
                lbsr    SINX0
                std     6,S
                lbsr    MULT
                lbsr    DIVIDE
                lbcs    CS400
                addd    XCENT
                lbvs    CS400
                std     XEND

                ldd     TH1             ; CALCULATE COORDINATES OF
                lbsr    SINX0           ; START OF ARC RELATIVE TO
                std     6,S             ; CENTRE AT (0,0) AND
                lbsr    MULT            ; RELATIVE TO CENTRE AT
                lbsr    DIVIDE          ; (XCENT,YCENT) MOVE CURSOR
                lbcs    CS400
                std     YP              ; TO START OF ACTUAL ARC
                addd    YCENT
                lbvs    CS400
                std     CPOSY
                ldd     TH1
                addd    #90             ; Ystart=YCENT+YP
                lbsr    SINX0           ; YP=RADIUS*SIN (TH1)
                std     6,S
                lbsr    MULT            ; Xstart=XCENT+XP
                lbsr    DIVIDE          ; XP=RADIUS*COS(TH1)
                bcs     CS400
                std     XP
                addd    XCENT           ; XP,YP RELATIVE TO CENTRE
                bvs     CS400
                std     CPOSX           ; AT (0,0) TEMP STORAGE

                std     XX
                cmpd    XEND            ; CHECK FOR COINCIDENCE OF START POINT AND END POINT
                bne     C200            ; IF THEY ARE THE SAME POINT SET (LASTX,LASTY) TO TRANSFORMED
                ldd     CPOSY           ; POSITION OF THIS POINT SO THAT WITH PMODE(2) COMMAND IN FORCE
                cmpd    YEND            ; THE CIRCLE DOES NOT PLOT THIS POINT TWICE
                bne     C200
                std     YY
                tst     TRFLAG
                bne     C100
                addd    YTRAN
                bvs     CS400
                std     LASTY
                ldd     CPOSX
                addd    XTRAN
                bvs     CS400
                std     LASTX
                bra     C200

C100            lbsr    XTRANS
                bcs     CS400
                std     LASTX           ; CALCULATE TRANSFORMED POSITION OF CURSOR AND STORE AS
                lbsr    YTRANS          ; (LASTX,LASTY)
                bcs     CS400
                std     LASTY

C200            clra
                sta     XP+2
                sta     YP+2

                leay    CIRTAB-5,PCR    ; PONIT TO RADIUS/STEP TABLE
                ldd     RADIUS
CLOOP1          leay    5,Y             ; SEARCH TABLE TO FIND SHIFT COUNTERS
                cmpd    ,Y              ; USED IN CIRCLE PLOTTING ALGORITM
                bgt     CLOOP1
                lda     2,Y
                sta     SHFT0
                sta     SHFCNT
                ldd     3,Y
                std     SHFT1

                ldd     TH2             ; CALCULATE NO. OF STEPS TO USE
                subd    TH1
C210            subd    #360
                bge     C210            ; GET IN RANGE 0-360
C220            addd    #360
                ble     C220

                std     6,S             ; NO. OF STEPS =2^N*(TH2-TH1)*2*PI/360
                ldd     #CIRCONS
                std     4,S             ; THIS IS APPROX
                lbsr    MULT            ; 2^N*(TH2-TH1)*1144/65536
CLOOP2          lsl     3,S             ; WHICH CAN BE DONE USING
                rol     2,S             ; SUBROUTINE MULT, SHIFTING RESULT LEFT
                rolb                    ; AND TAKING 2 MSB'S WITHOUT ROUNDING
                rola
                dec     SHFCNT
                bne     CLOOP2
                std     CCOUNT          ; STORE STEP COUNTER
                clra                    ; ENSURE CARRY CLEAR
                leas    9,S             ; RESTORE STACK
                rts

CS400           coma                    ; INDICATE ERROR
                leas    9,S             ; RESTORE STACK
                inc     OVRFLW
                rts
                opt     -l
                ttl     GPRINT          ; function
                opt     l
                pag
*
* SUBROUTINE GPRINT
*
*   TO PRINT TEXT CHARACTER ON GRAPHICS SCREEN. THE CHARACTER BIT PATTERN
*   IS OBTAINED FROM THE CHARACTER SET MODULE "CHRSET" WHICH MUST BE
*   LOADED. IT IS THEN CONVERTED INTO AN 8 x 10 ARRAY OF PIXEL COLOURS
*   WHICH IS DRAWN ON THE GRAPHICS SCREEN USING "GPUT".
*
* INPUT  :  BUFFER CONTAINS UP TO 10 TEXT CHARACTERS
*

GPRINT          leax    BUFFER,U        ; INITIALIZE READY FOR
                stx     BUFPON          ; SAVE POINTER AT START FOR NEXT BUFFER LOAD (IF ANY)
                ldb     #BCOUNT         ; DRAWING 10 TEXT CHARACTERS
                stb     COUNT
                ldy     LINKENT         ; START OF CHRSET TABLE

GPRLOOP         lda     ,X+             ; GET CHARACTER FROM BUFFER
                cmpa    #EOSEQ          ; IS IT CARRIAGE RETURN ?
                beq     GPR100          ; YES...SKIP
                pshs    X,Y
                suba    #'              ; NO...CONVERT TO CHRSET
                ldb     #CHRSIZ1        ; TABLE ENTRY
                mul
                leay    D,Y             ; CONVERT BIT PATTERN TO
                bsr     GPCONV          ; 'GPUT' TYPE ARRAY

                lbsr    PCHAR           ; PUT CHARACTER ON SCREEN

                puls    X,Y
                bcs     GPR100
                ldd     CPOSX           ; MOVE CURSOR TO START
                addd    #CHRSIZ2        ; OF NEXT CHARACTER
                std     CPOSX
                dec     COUNT           ; DECREMENT BUFFER COUNTER
                bne     GPRLOOP         ; BUFFER EMPTY ?...NO...CONTINUE
                lda     #BCOUNT
                sta     COUNT           ; SET UP COUNTER FOR NEXT BUFFER
                rts                     ; YES... GET NEXT BUFFER LOAD

GPR100          pshs    U               ; END OF STRING REACHED
                ldu     >D.Proc
                pshs    U
                ldu     >D.SysPrc
                stu     >D.Proc
                ldu     LINKADR         ; SO UNLINK CHRSET MODULE
                os9     F$Unlink
                puls    U
                stu     >D.Proc
                puls    U
                clr     COUNT
                clr     GPTFLAG         ; CLEAR GPRINT FLAG
                rts
                ttl     Convert         ; text character to GPUT array
                pag
*
* SUBROUTINE GPCONV
*
*   TO CONVERT 8 x 10 PIXEL PATTERN OF TEXT CHARACTER TO 4 x 10 (2 PIXELS
*   PER BYTE) COLOUR NUMBER ARRAY. INITIALLY ALL ARRAY VALUES ARE SET TO
*   CURRENT BACKGRAOUND COLOUR NUMBER, CORRESPONDING TO BIT=0 IN PIXEL
*   PATTERN. ANY PIXEL PATTERN BIT =1 CAUSES THE CURRENT DRAW COLOUR NUMBER
*   TO BE PLACED AT THE APPROPRIATE POINT IN THE ARRAY.
*
* INPUT  :  (Y) POINTS TO FIRST BYTE OF TEXT CHARACTER
*
* OUTPUT :  FIRST 40 BYTE OF GPBUFF CONTAIN 4 x 10 COLOUR NUMBER ARRAY
*

GPCONV          lda     DRCOL           ; FORM MS NIBBLE AND
                sta     DBMSK2          ; LS NIBBLE DRAW COLOUR MASKS
                lsla
                lsla
                lsla
                lsla
                sta     DBMSK1

                lda     #CHRARR         ; FORM DOUBLE BYTE
                sta     CCOUNT          ; 'GPUT' ARRAY
                lda     GPBGCOL         ; BACKGROUND COLOUR
                pshs    A               ; FILL MASK
                lsla
                lsla
                lsla
                lsla
                adda    ,S
                tfr     A,B
                leax    GPBUFF+40,U     ; AND FILL ARRAY
GPCLOOP         std     ,--X            ; WITH BACKGROUND COLOUR NUMBER
                dec     CCOUNT
                bne     GPCLOOP

                ldb     #CHRSIZ1        ; INITIALIZE BYTE COUNTER
                stb     CCOUNT+1
                ldb     ,X              ; GET FIRST BYTE FROM ARRAY

GPCLOOP1        lda     #CHRSIZ2        ; INITIALIZE BIT COUNTER
                sta     CCOUNT
                lda     ,Y+             ; GET NEXT BYTE OF PIXEL PATTERN
                clr     ,S              ; CLEAR LS NIBBLE FLAG

GPCLOOP2        lsla                    ; IS MS BIT SET
                bcc     GPC200          ; NO...SKIP
                tst     ,S              ; YES...IS THIS THE MS NIBBLE
                bne     GPC100          ; NO...SKIP
                andb    #$0F            ; YES...REPLACE MS NIBBLE
                orb     DBMSK1          ; WITH DRAW COLOUR
                bra     GPC200

GPC100          andb    #$F0            ; REPLACE LS NIBBLE
                orb     DBMSK2          ; WITH DRAW COLOUR

GPC200          tst     ,S              ; HAVE BOTH MS AND LS NIBBLES
                bne     GPC300          ; BEEN DONE ? YES...SKIP
                inc     ,S              ; NO...SET LS NIBBLE FLAG
                bra     GPC400          ; AND DONT STORE

GPC300          stb     ,X+             ; STORE IN ARRAY AND
                ldb     ,X              ; GET NEXT BYTE FROM ARRAY
                dec     ,S              ; CLEAR LS NIBBLE FLAG

GPC400          dec     CCOUNT          ; HAVE ALL 8 BITS BEEN DONE ?
                bne     GPCLOOP2        ; NO...CONTINUE
                dec     CCOUNT+1        ; HAVE ALL 10 BYTES BEEN DONE ?
                bne     GPCLOOP1        ; NO...CONTINUE
                leas    1,S
                rts                     ; ELSE RETURN
                ttl     Print           ; text character
                pag
*
* SUBROUTINE PCHAR
*
*   PRINTS A TEXT CHARACTER ONTO THE GRAPHICS SCREEEN USING PART OF "GPUT"
*   WHICH HONOURS CURRENT COORDINATE TRANSFORM (IF ANY). THE ROUTINE SETS
*   UP A STACK AS USED BY "GPUT" AND THEN BRANCHES INTO "GPUT" TO PLOT
*   THE CHARACTER ON THE GRAPHICS SCREEN.
*
* INPUT  :  GPBUFF CONTAINS THE CHARACTER PIXEL COLOUR NUMBERS
*           IN "GPUT" TYPE ARRAY.
*

PCHAR           leas    -19,S           ; GET STACK SPACE
                lda     #2
                sta     15,S            ; ASSUME X AND Y SCALE >1
                ldd     XSCALE
                bmi     PCH10
                lbsr    SCFAC           ; GET X AND Y SCALE FACTORS
                bra     PCH20

PCH10           coma
                comb
                addd    #1
                lbsr    SCFAC
                nega
PCH20           sta     13,S

                ldd     YSCALE
                bmi     PCH30
                lbsr    SCFAC
                bra     PCH40

PCH30           coma
                comb
                addd    #1
                lbsr    SCFAC
                nega
PCH40           sta     14,S

                ldd     CPOSX           ; GET CURSOR X POSITION
                tfr     D,Y
                addd    #CHRSIZ2-1      ; AND CALCULATE
                std     3,S             ; MAX X VALUE

                ldd     CPOSY           ; GET CURSOR Y POSITION
                std     9,S
                addd    #CHRSIZ1-1      ; AND CALCULATE
                std     5,S             ; MAXIMUM Y VALUE
                ldd     #CHRARR*2       ; ARRAY SIZE 40 BYTES
                std     11,S

                clr     ,S              ; CLEAR HALF BYTE FLAG
                clr     2,S             ; END FLAG
                clr     18,S
                dec     18,S            ; SET GPBUFF COUNTER TO 256
**
**  BRANCH TO GPUT
**

                leax    GPBUFF,U        ; POINT TO START OF ARRAY
                lbra    GPLOOP1         ; BRANCH INTO GPUT

                spc     10
                emod
GREND           equ     *
                end
