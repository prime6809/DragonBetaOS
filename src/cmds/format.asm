
                use     defsfile

tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     1

                mod     eom,name,tylg,atrv,start,size

LINELEN		equ	80

Statics         rmb     2		; Pointer to static variables
DevFID          rmb     1		; device file id
CurrentTrack    rmb     2		; current track
CurrentHead     rmb     2		; current head, note must immediately follow current head 
CurrentSec      rmb     1		; current sector
SecCounter      rmb     2		; sector counter when laying out track
LLFormatPtr0    rmb     2		; Low level data pointer for track 0
LLFormatPtr     rmb     2		; Low level data pointer for all other tracks
LLDataPtr       rmb     2		; temp low lever data pointer
DgnCoCo         rmb     1		; disk is Dragon/CoCo format
IsMFM           rmb     1		; is Double density (MFM)
IsDoubleTrk     rmb     1		; is double track distance
Sides           rmb     1
NoTracks        rmb     2		; number of tracks on disk
GoodSecCounter  rmb     1		; count of good sectors whilst verifying (overlaps SecCurrTrk).
SecCurrTrk      rmb     1		; sectors on track being laid out
SecPerTrk       rmb     2		; sectors per track
SecPerTrk0      rmb     2		; sectors on track 0 
PDDiskType      rmb     1		; Disk type (PD.TYP)
ReadyFlag       rmb     1		; are we ready flag ? 0=no, nz=yes
WordConv        rmb     2		; word converted by ASCII->binary input
Interleave      rmb     1		; sector interleave
ParamPtr        rmb     2		; Parameter pointer
TotalSectors    rmb     3		; total sectors on disk
SegAllocSize    rmb     1		; segment allocation size (sectors per cluster)
BitmapSize      rmb     2		; bitmap size in bytes
u0029           rmb     1
ClusterCount    rmb     1
RootSectors     rmb     1		; number of sectors in root dir
NoSysSectors    rmb     2		; number of system reserved sectors, LSN0, root etc
NoSysClusters   rmb     2		; number of system clusters
CurrentLSN      rmb     3		; current LSN during verify?
u0033           rmb     1
u0034           rmb     1
GoodSectors     rmb     2		; number of good sectors found by verify
u0037           rmb     2
u0039           rmb     2
u003B           rmb     1
BufPtr          rmb     2		; pointer to raw sector buffer
InitOffset      rmb     2		; offset of first sector ID in raw sector buffer
GapBetween      rmb     4		; gap between subsequent sector IDs in raw sector buffer
PhysVerify      rmb     1		; physical verify flag
DevTableEntPtr  rmb     3		; Dev table pointer
DriveNameBuf    rmb     32		; drive name buffer
DiskNameBuf     rmb     32		; disk name buffer
SecIDBuf        rmb     40		; buffer for sector IDs when working out interleave
Buffer        	rmb     256		; input / raw track / LSN0
OptBuf          rmb     256		; buffer for disk option area
u02B0           rmb     3
u02B3           rmb     9924
u2977           rmb     451

size            equ     .


name            FCS     "Format"        ; OS9 Module name

L0013           FCB     $12

L0014           FCB     $00,$00

L0016           FCB     $00,$00

L0018           FCB     $00,$00

; Hard disk track data
HDTrackDat      FCB     $80,$E5,$80,$E5,$00,$00

; Single desnity track data index marker
SDTrackDat      FCB     $01,$00		; Single density sector header		
		FCB	$28,$FF		; 40 bytes of $FF
		FCB	$06,$00		; 6 bytes of 0
		FCB	$01,$FC		; 1 byte of $FC (index mark)
                FCB     $0C,$FF		; 12 bytes of $FF
		FCB	$00,$00		; terminator (not written)

; sector header	+ data repeated 10 times			
		FCB	$06,$00		; 6 bytes of 0
		FCB	$01,$FE		; 1 byte of $FE (index marker)
                FCB     $04,$00		; Track, head, secno & secsize written here
		FCB	$01,$F7		; 1 bytes of $F7 (writes header CRC)
		FCB	$0A,$FF		; 10 bytes of $FF
		FCB	$06,$00		; 6 bytes of 0
		FCB     $01,$FB		; 1 byte of $FB (Data address mark)
		FCB     $80,$E5		; 256 bytes of Dummy data all $E5
                FCB     $80,$E5
		FCB     $01,$F7		; 1 bytes of $F7 (writes data CRC)
                FCB     $0A,$FF		; 10 bytes of $FF
		FCB	$00,$00
		
		FCB	$FF		; fill byte, to fill rest of track buffer
		FDB	$0043		; offset into raw sector buffer of first sector ID
                FDB     $0128		; number of bytes between sector ID blocks

; Double desnity track index marker
DDTrackDat      FCB     $50,$4E		; 80 bytes of $4E
		FCB     $0C,$00		; 12 bytes of 0
		FCB     $03,$F6		; 3 bytes of $F6 (writes $C2)
		FCB     $01,$FC		; 1 byte of $FC (index mark)
                FCB     $20,$4E		; 32 bytes of $4E
		FCB     $00,$00		; terminator (not written)
		
; sector header	+ data repeated 16 times	
		FCB	$0C,$00		; 12 bytes of 0
		FCB	$03,$F5		; 3 bytes of $F5 (writes $A1)
                FCB     $01,$FE		; 1 byte of $FE (id address mark)
		FCB     $04,$00		; Track, head, secno & secsize written here
		FCB     $01,$F7		; 1 bytes of $F7 (writes header CRC)
		FCB     $16,$4E		; 22 bytes of $4E
                FCB     $0C,$00		; 12 bytes of 0
		FCB     $03,$F5		; 3 bytes of $F5 (writes $A1)
		FCB     $01,$FB		; 1 byte of $FB (Data address mark)
		FCB     $80,$E5		; 256 bytes of Dummy data all $E5
                FCB     $80,$E5
		FCB     $01,$F7		; 1 bytes of $F7 (writes data CRC)
		FCB	$16,$4E		; 22 bytes of $4E
		FCB     $00,$00		; terminator (not written)
		
		FCB     $4E		; fill byte, to fill rest of track buffer
		FDB	$0090		; offset into raw sector buffer of first sector ID
		FDB	$0152		; number of bytes between sector ID blocks

; Dragon/CoCo Double desnity index marker 
DrCoTrackDat    FCB     $50,$4E		; 80 bytes of $4E
		FCB     $0C,$00		; 12 bytes of 0
		FCB     $03,$F6		; 3 bytes of $F6 (writes $C2)
		FCB     $01,$FC		; 1 byte of $FC (index mark)
                FCB     $20,$4E		; 32 bytes of $4E
		FCB     $00,$00		; terminator (not written)

; sector header	+ data repeated 18 times	
		FCB     $08,$00		; 8 bytes of 0
		FCB	$03,$F5		; 3 bytes of $F5 (writes $A1)
                FCB     $01,$FE		; 1 byte of $FE (id address mark)
		FCB     $04,$00		; Track, head, secno & secsize written here
		FCB     $01,$F7		; 1 bytes of $F7 (writes header CRC)
		FCB     $16,$4E		; 22 bytes of $4E
                FCB     $0C,$00		; 12 bytes of 0
		FCB     $03,$F5		; 3 bytes of $F5 (writes $A1)
		FCB     $01,$FB		; 1 byte of $FB (Data address mark)
		FCB     $80,$E5		; 256 bytes of Dummy data all $E5
                FCB     $80,$E5
		FCB     $01,$F7		; 1 bytes of $F7 (writes data CRC)
		FCB     $18,$4E		; 24 bytes of $4E
		FCB     $00,$00		; terminator (not written)
                
		FCB     $4E		; fill byte, to fill rest of track buffer
		
		FDB	$008C		; offset into raw sector buffer of first sector ID
		FDB	$0150		; number of bytes between sector ID blocks

start           stu     <Statics
                bsr     ClearVars	; clear some vars in u area
                bsr     ValidateOpenDev	; validate and open device path
                bsr     GetAllOpts	; get all options
                lbsr    CalcParms	; calculate format parameters
		lbsr    DoLLFormat	; do low level format
                lbsr    InitLSN0	; Initialise LSN0 on the formatted disk
                lbsr    ReadLSN0	; re-read LSN0 to verify it	
                lbsr    MakeBmVerify	; make bitmap and verify disk
                lbsr    MakeRoot	; make root directory

                ldu     <DevTableEntPtr
                os9     I$Detach
                clrb
Exit           	os9     F$Exit

ClearVars       leay    DevFID,U		; Point at first to clear
                pshs    Y		; save it on stack
                leay    Buffer,U		; point at last to clear
L00CE           clr     ,-Y		; clear a byte
                cmpy    ,S		; done all yet?	
                bhi     L00CE		; nope loop again

                puls    PC,Y            ; Restore and return

ValidateOpenDev	lda     ,X+		; get a byte from parameters
                cmpa    #'/'		; path seperator?
                beq     L00E2		; yes, so parse the name

L00DD           ldb     #E$BPNam	; bad path name error
                lbra    PrintErrorExit	; exit

L00E2           os9     F$PrsNam	; parse the name of dev to format
                lblo    PrintErrorExit	; error, exit

                lda     #'/'		; check for path beyond dev name?
                cmpa    ,Y		
                beq     L00DD		; yes, bad path, exit

                sty     <ParamPtr	; save pointer to remaining params
                leay    <DriveNameBuf,U	; point at drive name buffer
L00F5           sta     ,Y+		; save a char from path
                lda     ,X+		; get next char
                decb			; decrement counter
                bpl     L00F5		; loop if more to do

                leax    <DriveNameBuf+1,U	; point at drive name
                lda     #$20		; terminate it with a space
                sta     ,Y		
                clra			; flag using device capabilities		
                os9     I$Attach	; attach to device
                lblo    PrintErrorExit	; exit on error

                stu     <DevTableEntPtr	; save pointer to device table		
                ldu     <Statics	; point at statics again	
                lda     #'@'		; terminate device name with codes
                ldb     #' '		; to open whole device
                std     ,Y		; store in dev name
                lda     #WRITE.		; write access
                leax    <DriveNameBuf,U	; point at dev name
                os9     I$Open		; open it
                blo     Exit		; exit on error

                sta     <DevFID		; save file ID
                rts
		
GetAllOpts      bsr     GetPDOpts	; get drive opts from path descriptor
                bsr     GetOptions	; get options from command line
                lbsr    GetOptionsKbd	; get optiions from keyboard
                rts

GetPDOpts       leax    OptBuf,U	; point to option buffer
                clrb			; function SS.Opt
                os9     I$GetStt	; go get options 
                blo     Exit		; exit on error

                ldb     (PD.SID-PD.OPT),X	; get number of sides
                stb     <Sides			; save it
                ldb     (PD.DNS-PD.OPT),X	; get density
                pshs    B			; save 
                andb    #DNS.MFM		; is it MFM?
                stb     <IsMFM			; yes save it
		
                puls    B		; restore density byte
                lsrb			; shift left
                andb    #$01		; is itDouble track distance?
                stb     <IsDoubleTrk	; save it
		
                ldd     (PD.CYL-PD.OPT),X	; get no of cylinders
                std     <NoTracks		; save it
                
		ldb     (PD.TYP-PD.OPT),X	; get disk type byte
                stb     <PDDiskType		; save it
                andb    #TYP.CCF		; Dragon / CoCo format?
                stb     <DgnCoCo		; save it
                beq     L0157			; branch if not

                stb     <IsMFM			; flag mfm
L0157           ldd     (PD.SCT-PD.OPT),X	; get sectors / track
                std     <SecPerTrk		; save it
                tst     <DgnCoCo		; is this a DragonCoCo disk?
                bne     L0161			; yes, skip, they have same t0s as other tracks

                ldd     (PD.T0S-PD.OPT),X	; get track 0 sectors
L0161           std     <SecPerTrk0		; save it
                
		ldb     (PD.ILV-PD.OPT),X	; get sector interleave
                stb     <Interleave		; save it
		
                ldb     #$01			; assume an SAS of 1
                stb     <SegAllocSize
                ldb     (PD.SAS-PD.OPT),X	; get segment allocation size
                beq     L017B			; zero skip ahead

                stb     <SegAllocSize		; save allocation size
                negb				; validate it
                decb	
                andb    <SegAllocSize
                beq     L017B			; valid skip ahead

                ldb     #$01			; otherwise revert to SAS=1
                stb     <SegAllocSize
L017B           clrb				; no error 
                rts				; return
		
GetOptions           ldx     <ParamPtr	; point at parameters
L017F           leay    OptionTable,PCR	; point at option table
                bsr     L0196		; check for option

                blo     L019F		; end of table, error return		

                pshs    d		; save d
                ldd     2,Y		; get offset from table
                leay    D,Y		; add to y
                puls    d		; restore d
                jsr     ,Y		; jump to handler routine

                bhs     L017F		; no error, check for more options

                lbra    Exit		; error return

L0196           lda     ,X+		; get character from table
L0198           cmpa    ,Y		; is the current paramater char the same?
                bne     L01A0		; no, try next entry

                ldb     1,Y		; pick up byte from table
                clra			; clear a
L019F           rts			; return

L01A0          	leay    <<OptionEntLen,Y	; move table pointer to next option
                tst     ,Y		; end of table ?
                bne     L0198		; no, check next entry
	
                coma			; flag end of table 
                rts			; return

;
; Option table for processing command line optiions.
; Each entry consists of :
; Offset	Size	Use
; 0		1	token character to match to command line
; 1		1	byte to be passed in b to handler routine
; 2		2	offset to handler, relative to the start of this entry.
;

OptionTable     FCC     "R"
                FCC     "Y"
                FDB     SetReadyFlag-OptionTable 	
OptionEntLen	equ	*-OptionTable

OptRL           FCC     "r"
                FCC     "Y"
                FDB     SetReadyFlag-OptRL 

OptQu           FCC     /"/
                FCB     $00
                FDB     SetDiskName-OptQu

OptCl           FCC     ":"
                FCB     $00
                FDB     SetInterleave-OptCl

OptSU           FCC     "S"
                FCB     $00
                FDB     SetDensity-OptSU

OptSL           FCC     "s"
                FCB     $00
                FDB     SetDensity-OptSL

OptDU           FCC     "D"
                FCB     $01
                FDB     SetDensity-OptDU

OptDL           FCC     "d"
                FCB     $01
                FDB     SetDensity-OptDL

Opt1            FCC     "1"
                FCB     $01
                FDB     SetSides-Opt1

Opt2            FCC     "2"
                FCB     $02
                FDB     SetSides-Opt2

OptSq           FCC     "'"
                FCB     $00
                FDB     SetNumTracks-OptSq

OptSla          FCC     "/"
                FCB     $00
                FDB     SetSAS-OptSla

OptOpb          FCC     "("
                FCB     $00
                FDB     SkipChar-OptOpb

OptClb          FCC     ")"
                FCB     $00
                FDB     SkipChar-OptClb

OptCom          FCC     ","
                FCB     $00
                FDB     SkipChar-OptCom

OptSpc          FCC     " "
                FCB     $00
                FDB     SkipChar-OptSpc

                FCB     $00

SetDensity      tst     <DgnCoCo	; are we formating a Dragon or CoCo disk?
                bne     SkipChar	; yes, skip

                stb     <IsMFM		; set MFM flag
SkipChar        rts			; return

SetReadyFlag    stb     <ReadyFlag	; set ready flag
		rts			; return

SetSides        stb     <Sides		; set sides
                rts

SetDiskName     leay    <DiskNameBuf,U	; point at disk name buffer
                ldb     #$20		; at max 32 characters
L01FB           lda     ,X+		; get a char from command line
                cmpa    #'"'		; close quote?
                beq     L0212		; yes skip

                sta     ,Y+		; save in disk name buffer
                decb			; decrement count
                bne     L01FB		; loop again if nonzero

L0206           ldb     ,X+		; get next from command line
                cmpb    #'"'		; quote?
                beq     L0218		; yes backup one and terminate

                cmpb    #$20		; disk name empty?
                bhs     L0206		; yes, try next

                bra     L0218		; backup and terminate

L0212           lda     #$20		; space char
                cmpb    #$20		; no disk name?
                beq     L021C		; yes just terminate with space

L0218           leay    -1,Y		; backup one in buffer
                lda     ,Y		; get character there
L021C           adda    #$80		; set top bit to flag end
                sta     ,Y		; put char in buffer
                clrb			; no error
                rts			; return

SetNumTracks    lbsr    ReadWord	; read word from command line	

                ldd     <WordConv	; get read word
                std     <NoTracks	; update number of tracks
                rts

SetInterleave   lbsr    ReadWord	; read word from command line	

                ldd     <WordConv	; get read word
                tsta			; is msb zero?
                beq     L0234		; yep, then it's valid

                ldb     #$01		; otherwise set it to 1
L0234           stb     <Interleave	; save new interleave
		
L0236           rts

SetSAS          lbsr    ReadWord	; read word from command line	

                ldd     <WordConv	; get read word
                tsta			; check that msb is zero
                beq     L0241		; yes skip

                ldb     #$01		; no default to SAS=1
L0241           stb     <SegAllocSize	; save it
                negb			; validate it
                decb
                andb    <SegAllocSize
                beq     L024D		; valid skip

                ldb     #$01		; ofthwsise default to SAS=1
                stb     <SegAllocSize	; save it
L024D           clrb
                rts
		
GetOptionsKbd   leax    TitleMsg,PCR	; point at title message
                lbsr    Write80		; write it

L0256           leay    OptBuf,U
                ldx     (PD.T0S-PD.OPT),Y	; get track 0 sectors
                tst     <IsMFM		; is this an MFM disk?
                beq     L0262		; no skip on

                ldx     (PD.SCT-PD.OPT),Y	; get sectors per track
L0262           stx     <SecPerTrk	; update sec / track
                lbsr    L090F

                leax    FormatMsg,PCR	; point to formatting message
                ldy     #FormatMsgL	; get it's length
                lbsr    WriteLenY	; write it

                leax    <DriveNameBuf,U	; point to drive name
                tfr     X,Y		; copy ptr	
L0277           lda     ,Y+		; get a byte from it
                cmpa    #'@'		; check for @		
                bne     L0277		; geep going if not found

                pshs    Y		; save y
                lda     #$0D		; terminate name string
                sta     -1,Y
                lbsr    Write80		; go write it to screen

                puls    Y		; pull end of disk pointer
                lda     #'@'		; replace the @
                sta     -1,Y
                lda     <ReadyFlag	; check the ready flag
                bne     L0236		; if set, don't prompt, assume ready

L0290           leax    ReadyMsg,PCR	; point to ready message
                ldy     #ReadyMsgL	; and it's length
                lbsr    PromptGetKey	; prompt user and read key

                anda    #$DF		; force it uppercase
                cmpa    #'Y'		; check user typed Y, for ready
                beq     L0236		; yep, return

                clrb			; flag no error
                cmpa    #'Q'		; quit?
                lbeq    Exit		; yep exit

                cmpa    #'N'		; not ready yet?
                bne     L0290		; so prompt again

                pshs    B,A		
                clr     <IsMFM		; set fm
                tst     <PDDiskType	; test disk type
                bpl     L02B8		; branch if it is a floppy?

                clrb			; no error
                lbra    Exit		; exit

L02B8           tst     <DgnCoCo	; is it a dragon/coco disk ?
                bne     L02D0		; yes, skip

                leax    DoubleDenMsg,PCR	; point at double desnity message
                ldy     #DoubleDenMsgL	; and it's length
                bsr     PromptGetKey	; go prompt and read a key

                anda    #$DF		; convert to upper case
                cmpa    #'N'		; not double density?
                beq     L02D4		; skip on

                cmpa    #'Y'		; user wants double density
                bne     L02B8		; loop if neither 'Y' or 'N'

L02D0           lda     #$01		; flag double density format
                sta     <IsMFM
		
L02D4           tst     <IsDoubleTrk	; check to see if double track
                beq     L02EE		; nope, skip on

                leax    ChangeTPIMsg,PCR	; point to double track message
                ldy     #ChangeTPIMsgL	; and length	
                bsr     PromptGetKey	; go prompt and read a key

                anda    #$DF		; convert to upper case
                cmpa    #'N'		; dir user indicate no
                beq     L02EE		; if so skip ahead

                cmpa    #'Y'		; dir user indicate yes
                bne     L02D4		; loop if neither 'Y' or 'N'

                clr     <IsDoubleTrk	; clear double track flag
		
L02EE           lda     #$01		; assume single sided
                sta     <Sides
                leax    DoubleSideMsg,PCR	; point at sides message
                ldy     #DoubleSideMsgL	; and length
                bsr     PromptGetKey	; go prompt and read a key

                anda    #$DF		; convert to upper case
                cmpa    #'N'		; dir user indicate no
                beq     L030A		; branch if so

                cmpa    #'Y'		; dir user indicate yes
                bne     L02EE		; loop if neither 'Y' or 'N'

                lda     #$02		; set sides to 2
                sta     <Sides
		
L030A           leax    HowManyCylMsg,PCR	; point at tracks message
                ldy     #HowManyCylMsgL	; and length
                bsr     WriteLenY	; go write it

                leax    Buffer,U	; point at input buffer
                ldy     #$0006		; read up to 6 bytes
                lda     #$01		; stdin
                os9     I$ReadLn	; go read them
                lblo    Exit		; error, exit

                lbsr    ReadWord	; go read a word from buffer

                ldd     <WordConv	; get input word
                std     <NoTracks	; update number of tracks
                leas    2,S		; clean up stack
                lbra    L0256

WriteEol        leax    L0AAA,PCR	; write an eol to screen

Write80		ldy     #LINELEN	; Write a linlen sized bufffer
					; Enter here if passing length in Y
WriteLenY       lda     #1		; stdout
                os9     I$WritLn	; go write it
                rts			

PromptGetKey    pshs    U,Y,X,D		; save regs
                bsr     WriteLenY	; write passed message

                leax    ,S		; point at top of stack
                ldy     #1		; read one character
                clra			; stdin
                os9     I$Read		; go read it
                lblo    Exit		; error, exit

                bsr     WriteEol

                puls    U,Y,X,D		; restore regs
                anda    #$7F		; make sure it's ASCII
                rts

; Calculate format parameters, get pointers to sector tables etc		
CalcParms       leax    HDTrackDat,PCR	; point at HD track table
                stx     <LLFormatPtr0	; save it	
                ldb     <PDDiskType	; get disk type
                bitb    #TYP.HARD+TYP.NSF	;hard disk or non-standard?
                bne     L037C		; yep, skip

                leax    SDTrackDat,PCR	; point at single density track data
                stx     <LLFormatPtr0	; save pointer
                tst     <IsMFM		; is this a double density disk?
                beq     L037C		; no, skip

                leax    DDTrackDat,PCR	; point at double density track data
                tst     <DgnCoCo	; is this a Dragon or CoCo disk?
                beq     L037C		; no skip

                leax    DrCoTrackDat,PCR ; point at Dragon/CoCo track data	
                stx     <LLFormatPtr0	; save it
		
L037C           stx     <LLFormatPtr	; save pointer for all tracks
                clra			; get sides as 16 bit....
                ldb     <Sides
                tfr     D,Y		; save it to Y
                clrb
                ldx     <NoTracks	; get number of tracks
                bsr     MulBXbY		; multiply tracks by sides
; Total number of tracks to format in b:x		
                exg     D,X		; subtract 1 from total as we
                subd    #$0001		; will handle t0 seperately
                bhs     L0391

                leax    -1,X
		
L0391           exg     D,X
                ldy     <SecPerTrk	; get number of sectors in b
                bsr     MulBXbY		; multiply by number of tracks (except t0)

                exg     D,X		
                addd    <SecPerTrk0	; add number of t0 sectors
                std     <TotalSectors+1	; save total sectors	
                exg     D,X
                adcb    #$00
                stb     <TotalSectors	

; Work out the cluster size		
                lda     #$08		; divisor
                pshs    A		; save it
                ldx     <TotalSectors+1	; get total sectors
                ldb     <TotalSectors
                bsr     Div24by8	; divide by 8

                lda     <SegAllocSize	; get allocation size
                pshs    A		; save it
                bsr     Div24by8

                tstb			; more than $ffff bytes required?	
                beq     L03C1		; branch if so

                leax    CluSizeMisMsg,PCR	; point to cluster size mismatch message 
                lbsr    Write80			; write it

                lbra    AbortFmtExit	; Prit format aborted and exit 

L03C1           leas    2,S		; clean up stack	
                stx     <BitmapSize	; save bitmap size
                rts

; Multiply B:X * Y
MulBXbY         lda     #$08		; clear 8 bytes
L03C8           clr     ,-S		; move sp down, clear a byte
                deca			; decrement counter
                bne     L03C8		; loop if more


                sty     ,S		
                stb     2,S		
                stx     3,S		
L03D4           ldd     ,S		
                beq     L03F7		

                lsra			
                rorb
                std     ,S		
                bhs     L03EA

                ldd     3,S		
                addd    6,S		
                std     6,S
                lda     2,S		
                adca    5,S
                sta     5,S
L03EA           ldd     3,S
                lslb
                rola
                std     3,S
                lda     2,S
                rola
                sta     2,S
                bra     L03D4

L03F7           leas    5,S
                puls    PC,X,B          
		
; 24 bit divide (2,s = divisor, B:X = dividend, result in B:X)		
L03FB           pshs    X,B
                lsr     ,S
                ror     1,S
                ror     2,S
                puls    X,B
                exg     D,X
                adcb    #$00
                adca    #$00
                exg     D,X
                adcb    #$00
Div24by8        lsr     2,S
                bne     L03FB
                rts


; prompt user to confirm and then do the low level part of the format
DoLLFormat      tst     <PDDiskType	; get disk type
                bpl     L042E		; branch if floppy

                leax    PhyLogMsg,PCR	; get pointer to physical and logical message
                ldy     #PhyLogMsgL	; and length
                lbsr    PromptGetKey	; go prompt and get key

                anda    #$DF		; convert to upper case
                cmpa    #'Y'		; dir user indicate yes
                beq     L042E		; yep skip on

                cmpa    #'N'		; dir user indicate no
                bne     DoLLFormat	; no loop again...

                rts			; return, only logical format
		
L042E           lda     <DevFID		; get device file id
                ldb     #SS.Reset	; reset the device
                os9     I$SetStt	; go and do it
                lblo    Exit		; error, exit

                ldd     #0		; current track = 0
                std     <CurrentTrack
                inca			; get current sector
                sta     <CurrentSec	; save it
		
L0441           clr     <CurrentHead	; reset to head 0
L0443           bsr     LayoutTrack	; layout in memory copy of raw track

                leax    Buffer,U	; point to buffer
                ldu     <CurrentTrack	; get current track
		
                clrb			
                tst     <IsMFM		; is disk DD/MFM ?
                beq     L045E		; nope skip

                tst     <DgnCoCo	; is it Dragon or CoCo ?	
                bne     L045C		; yep skip

                tst     <CurrentTrack+1
                bne     L045C

                tst     <CurrentHead	; are we on head 0 ?
                beq     L045E		; yes skip

L045C           orb     #$02		
L045E           tst     <IsDoubleTrk	; double track step?
                beq     L0464		; nopw, skip

                orb     #$04		; flag double step
L0464           lda     <CurrentHead	; get current head
                beq     L046A		; skip if zero

                orb     #$01		; flag head 0
L046A           tfr     D,Y		; move flags / heads to y

                lda     <DevFID		; get device file id
                ldb     #SS.WTrk	; write a track
                os9     I$SetStt	; go do it
                lblo    Exit		; error, exit

                ldu     <Statics	; get pointer to statics		
                ldb     <CurrentHead	; get current head
                incb			; increment it
                stb     <CurrentHead	; save it back
                cmpb    <Sides		; done all heads ?
                blo     L0443		; no loop again

                ldd     <CurrentTrack	; get current track
                addd    #$0001		; icrement it
                std     <CurrentTrack	; save it
                cmpd    <NoTracks	; done all tracks?	
                blo     L0441		; no loop again

                rts			; otherwise finished
		
L048F           ldy     <LLDataPtr
WriteIndex      ldd     ,Y++		; get a word from table
                beq     L04AC		; end of table, exit

; put byte b in buffer a times.
L0496           stb     ,X+		; but byte in buffer
                deca			; dec count
                bne     L0496		; loop again if not zero

                bra     WriteIndex	; do next entry

;
; Lays out raw track data in RAM according to the format specified in the 
; WD277x controller documentation.
; 
; A table of value / repeat pairs (see SDTrackDat, DDTrackDat and DraCoTrackDat) is 
; first used to set up the track's data in the buffer, as this can be done with simple
; loops.
; Once this is done the order of the sector ID's is calculated taking interleave into 
; account.
; These sector IDs along with the correct values for head and track are then patched
; into the raw data table writen to memory in the first stage.
;
; This data is then passed bact to DoFormat, which sends it to the controller. 
;

LayoutTrack     lda     <PDDiskType	; get disk type
                bita    #TYP.HARD+TYP.NSF	;hard disk or non-standard?		
                beq     L04AD		; no skip ahead

                ldy     <LLFormatPtr	; point at format data
                leax    Buffer,U	; point at buffer
                bsr     WriteIndex	; go write it

L04AC           rts


; check to see if we are on head 0 side 0, and if so use the appropriate set
; of format data (which may be different from rest of disk).
L04AD           ldy     <LLFormatPtr	; point at format data
                ldb     <SecPerTrk+1	; get lsb of sec/track
                tst     <DgnCoCo	; is this a Dragon or CoCo disk?	
                bne     L04C3		; yes skip

                tst     <CurrentTrack+1	; lsb of current track 0? 
                bne     L04C3		; nope, skip

                tst     <CurrentHead	; check for head 0
                bne     L04C3		; nope. skip

                ldy     <LLFormatPtr0	; track 0 haed 0, get pointer to it's data
                ldb     <SecPerTrk0+1	; get sectors on track 0
		
L04C3           sty     <LLDataPtr	; save current low level data pointer
                stb     <SecCounter+1	; save in working sector counter		
                stb     <SecCurrTrk
                bsr     SetupSecIDs	; setup sector ids for this track

                leax    Buffer,U	; point at track buffer
                bsr     WriteIndex	; write track index	

                sty     <LLDataPtr	; save pointer to beginning of ll sector data
L04D5           bsr     L048F		; write the data to the buffer

                dec     <SecCounter+1	; decrement sector counter
                bne     L04D5		; branch if not zero	

                lda     ,Y+		; get next byte
                sty     <LLDataPtr	; update low level pointer
                stx     <BufPtr		; save current buffer pointer
		
                leay    u2977,U		; point to end of buffer
                sty     <WordConv	; save it	
                tfr     A,B		; make b=a
L04EB           std     ,X++		; write bytes
                cmpx    <WordConv	; reached end of buffer	
                blo     L04EB		; no loop again

                ldy     <LLDataPtr	; recover low level data pointer
                ldd     ,Y++		; get next two words, and save them
                std     <InitOffset	; these are initial offset into the raw buffer ID
                ldd     ,Y		; of first sector and the gap between 
                std     <GapBetween	; subsequent sector IDs
		
                clr     <SecCounter+1	; clear sector counter
                leax    Buffer,U	; point back at beginning of buffer
                ldd     <InitOffset	; get the initial offset to the first raw sector's header		
                leay    SecIDBuf,U	; point to the beginning of the interleaved sector IDs		
		
L0508           leax    D,X		; move to the first ID in the buffer
                ldd     <CurrentTrack+1	; get the current track and head
                std     ,X		; save in sector ID
                ldb     <SecCounter+1	; get current sector number 
                lda     B,Y		; get id from interleaved sector ID buffer
                incb			; move to next sector number
                stb     <SecCounter+1	; save it back
                ldb     <CurrentSec	; get current sector no
                tst     <DgnCoCo	; is this a Dragon/CoCo disk?
                beq     L051C		; no, skip

                inca			; increment id
L051C           std     2,X		; save it in sector header
                lda     <SecCounter+1	; get current sector no
                cmpa    <SecCurrTrk	; done all sectors?
                bhs     L0528		; yes, return

                ldd     <GapBetween	; get the gap between sector IDs
                bra     L0508		; and do the next

L0528           rts

; y=lldata ptr, b=sectors/track
; setup sector IDs for required interleave
SetupSecIDs     pshs    Y,B		; save regs
                tfr     B,A		; sec/track in a
                ldb     <CurrentTrack+1	; get lsb of current track
                cmpb    #$01		; higher than 1?
                bhi     L0555		; correct layout already in buffer, exit

                leax    SecIDBuf,U	; point to the beggining of ID buffer
                leay    A,X		; get offset into buffer of last sector
                ldb     <Interleave	; get interleave
                bne     L0544		; non zero skip
	
AbortIntExit    leax    AbortIntMsg,PCR	; exit, invalid interleave
                lbra    MesExit

L0544           cmpb    <SecCurrTrk	; greater thn sec/track ?
                bhi     AbortIntExit	; yes, exit

                nega			
                pshs    Y,X,B,A
                clra
; Buf offs last secL
; Buf offs last secH
; Buf offs start sec L
; Buf offs start sec H
; Interleave
; -ve sectrk <--SP		
		
L054C           sta     ,X		; save sector no in buffer
                inca			; increment
                cmpa    <SecCurrTrk	; done all ?
                bne     L0557		; nope skip on

                leas    6,S		; clean up stack
L0555           puls    PC,Y,B          ; restore and return

L0557           ldb     <Interleave	; get interleave
                abx			; add to buffer pointer
                cmpx    4,S		; past end of buffer?
                blo     L0562		; no skip

                ldb     ,S		; get -ve sectors / track
                leax    B,X		; adjust buffer pointer, loop round
		
L0562           cmpx    2,S		; at first sector?
                bne     L054C		; no skip

                leax    1,X		; add one
                stx     2,S		; save as new first
                bra     L054C		; loop again

InitLSN0        lbsr    ClearBuf	; clear buffer

                ldd     <TotalSectors+1	; get total sectors 
                std     DD.TOT+1,X	; and save in LSN0 bits 0..15
                ldb     <TotalSectors	; bits 16-24
                stb     ,X
                ldd     <SecPerTrk	; get sectors per track & tracks
                std     <DD.SPT,X
                stb     DD.TKS,X
                lda     <SegAllocSize	; get SAS
                sta     DD.BIT+1,X	
		
                clra			
                ldb     <BitmapSize	; calculate no of bitmap sectors needed
                tst     <BitmapSize+1	; exact multiple of sector size?
                beq     L058C		; yes, skip no extras needed

                addd    #$0001		; add extra sector for bytes at the end
L058C           addd    #$0001		
                std     DD.DIR+1,X	; save directory sector
                
		clra			
                tst     <IsMFM		; is this double density
                beq     L0598		; no, skip

                ora     #FMT.DNS	; mark double density		
L0598           ldb     <Sides		; get sides
                cmpb    #$01		; only one side?
                beq     L05A0		; yes, skip

                ora     #FMT.SIDE	; mark it as double sided
L05A0           tst     <IsDoubleTrk	; is it double tracked ?
                beq     L05A6		; no...skip

                ora     #FMT.TDNS	; flag if so
L05A6           sta     <DD.FMT,X	; save format byte
                
		ldd     <BitmapSize	; get bitmap size
                std     DD.MAP,X	; save it
		
                lda     #$FF		; attributes
                sta     DD.ATT,X
                leax    Buffer+DD.DAT,U	; point to time buffer
                os9     F$Time		; get creation time
		
                leax    Buffer+DD.NAM,U	; point to disk name in LSN0
                leay    <DiskNameBuf,U	; point to saved disk name
                tst     ,Y		; null name?
                beq     L05CB		; yes, skip

L05C3           lda     ,Y+		; transfer a byte
                sta     ,X+
                bpl     L05C3		; keep going until end of name (ms bit set).

                bra     L05FE

L05CB           leax    DiskNameMsg,PCR	; point to disk name message
                ldy     #DiskNameMsgL	; and length
                lbsr    WriteLenY	; go write it

                leax    Buffer+DD.NAM,u	; point to disk name buffer	
                ldy     #33		; max no of bytes 
                clra			; stdin
                os9     I$ReadLn	; go read them
                bhs     L05EF		; no error, skip on

                cmpa    #E$EOF		; end of file?
                bne     L05CB		; no, loop again

AbortFmtExit    leax    FormatAbtMsg,PCR	; print format aborted and exit
                lbra    MesExit

L05EF           tfr     Y,D		; get number of characters read
                leax    D,X		; point to last character
                clr     ,-X		; clear it and point at previous
                decb			; decrement count
                beq     L05CB		; no characters entered prompt again

                lda     ,-X		; get last character
                ora     #$80		; set bit 7
                sta     ,X		; put it back
		
L05FE           leax    Buffer+DD.DAT,U	; point to time
                leay    <64,X		
                pshs    Y		; save y
                ldd     #0		; zero d
L060A           addd    ,X++		; add some randomness to d
                cmpx    ,S		; done 64 bytes?
                blo     L060A		; no, loop again

                leas    2,S		; cleanup stack
                std     Buffer+DD.DSK,U	; save as random disk id.
		
; Neiter I not the NitrOS9 developers are sure what this code does,
; it moves 3 words from this format program into the 3 words at LSN0
; offset $F0,$F2 and $F4, they are set to 0 in this version of the program
; They seem to also be in the Tandy format program from which the NitrOS9 
; format is derrived from. Wonder if it was meant to be set to some sort
; of licence derrived number, so you can tell who had formatted what disk?
; we will probably never know!
 		
                ldd     L0014,PCR
                std     Buffer+$F0,U
                ldd     L0016,PCR
                std     Buffer+$F2,U
                ldd     L0018,PCR
                std     Buffer+$F4,U
                
		lda     <DevFID		; get device file id
		ldb     #SS.OPT		; get option area 		
		leax    Buffer+DD.OPT,u	; point at option area in LSN0
                os9     I$GetStt	; go get it
		
                ldb     #SS.Reset	; reset the drive
                os9     I$SetStt	; go do it	
                lblo    Exit		; error, exit

                leax    Buffer,U	; point to buffer
                lbra    WriteSec	; go write it

ReadLSN0	lda     <DevFID		; get the device file id
                os9     I$Close		; close it
		
                leax    <DriveNameBuf,U	; point to drive name
                lda     #READ.		; open drive for read
                os9     I$Open		; go open it
                lblo    BadSysExit	; exit on error

                sta     <DevFID		; save file id
                leax    Buffer,U	; point to buffer
                ldy     #$0100		; read one sector (LSN0)
                os9     I$Read		; go read it	
                lblo    BadSysExit	; error, exit

                lda     <DevFID		; get device file id
                os9     I$Close		; close it
		
                leax    <DriveNameBuf,U	; point to drive name
                lda     #UPDAT.		; open for read/write	
                os9     I$Open		; go open it
                lblo    BadSysExit	; exit on error

                sta     <DevFID		; save device file id
                rts
		
MakeBmVerify    lda     <PDDiskType	; get disk type
                clr     <PhysVerify		
                bita    #TYP.HARD	; is it a hard disk ?
                beq     L069E		; no......

L0687           leax    PhyVerifyMsg,PCR ; point to physical verify message
                ldy     #PhyVerifyMsgL	; and length
                lbsr    PromptGetKey	; go get response from user

                anda    #$DF		; make it upper case
                cmpa    #'Y'		; did user indicate yes
                beq     L069E		; if so go do verify

                cmpa    #'N'		; did user indicate no
                bne     L0687		; no loop again

                sta     <PhysVerify	; save flag
		
L069E           ldd     <SecPerTrk0	; get sectors on track 0
                std     <GoodSecCounter
                clra			; d=0
                clrb
                std     <GoodSectors	; zero some vars	
                std     <CurrentTrack
                std     <SecCounter
                std     <CurrentLSN+1
                stb     <CurrentLSN
                sta     <u003B
		
                leax    OptBuf,U	; point to option buffer
                stx     <u0037
                lbsr    ClearBufX	; clear 256 bytes at there

                leax    256,X		; buf ptr move on by one sector
                stx     <u0039
                
		clra			; D=1
                ldb     #$01
                std     <u0033
                lda     <SegAllocSize	; get allocation size
                sta     <ClusterCount	; save in cluster count
                clr     <u0029
		
                clra
                ldb     <BitmapSize	; get number of sectors in bitmap
                tst     <BitmapSize+1	; exact no of sectors in pitmap?
                beq     L06D4		; yes, skip

                addd    #$0001		; otherwise add one for the extra
L06D4           addd    #9		; add 8 sectors for root 
                std     <NoSysSectors	; save number of system sectors

; Since cluster sizes can only be a power of 2 (1,2,4,8,16 etc) we divide block count
; by 2 until we get a carry, this gives us the cluster count
 		
                lda     <SegAllocSize	; get allocation size
L06DB           lsra			; divide by 2	
                blo     L06EC		; branch on overflow ?

                lsr     <NoSysSectors	; shift sys sectors right
                ror     <NoSysSectors+1	; dividing it by 2
                bhs     L06DB		; keep going until we get a carry

                inc     <NoSysSectors+1	; addjust for the carry
                bne     L06DB		; if non-zero we don't need to touch MSB

                inc     <NoSysSectors	; increment msb
                bra     L06DB		; loop again

L06EC           ldb     <NoSysSectors+1	; update number of system clusters	
                stb     <NoSysClusters
                lda     <SegAllocSize	; get allocation size
                mul			; multiply by no clusters
                std     <NoSysSectors	; save back in number of system sectors
                
		subd    #$0001		; calculate number of sectors in root
                subb    <BitmapSize	; take away side of bitmap
                sbca    #$00		; carry
                tst     <BitmapSize+1	; bitmap size exact no of sectors?
                beq     L0703		; yes skip

                subd    #$0001		; suptract an extra sector
L0703           stb     <RootSectors	; save sectors in root

L0705           tst     <PhysVerify	; should we verify 
                bne     L0737		; no skip

                lda     <DevFID		; get device file id
                leax    Buffer,U	; point to buffer
                ldy     #$0100		; one sector's worth
                os9     I$Read		; go read it
                bhs     L0737		; no error, skip

                os9     F$PErr		; print error
                lbsr    GetNextSec	; get next sector		

                lda     #$FF
                sta     <u0029
                tst     <CurrentLSN
                bne     L0737

                ldx     <CurrentLSN+1
                cmpx    <NoSysSectors	; done all?
                bhi     L0737		; yes, skip

BadSysExit	leax    BadSysSecMsg,PCR	; point to bad system sector message
MesExit         lbsr    Write80			; write it

                clrb			; flag no error
                lbra    Exit		; exit

L0737           ldd     <SecCounter	; get sector counter		
                addd    #$0001		; add one to it
                std     <SecCounter	; save it back
                cmpd    <GoodSecCounter	; check against good sector count
                blo     L077C		

                clr     <SecCounter	; clear counted sectors
                clr     <SecCounter+1
                tst     <PhysVerify	; should we verify?
                bne     L0771		; no, skip

                lda     #$20		; get a space & save		
                pshs    A
                lda     <CurrentTrack+1	; get current track LSB
                lbsr    HexDigits	; convert it to hex

                pshs    D		; save it on stack
                lda     <CurrentTrack	; get current track MSB
                lbsr    HexDigits	; convert it to hex

                pshs    B		; save only LSB
                tfr     S,X		; point X to saved track number (on stack)
                ldy     #4		; output 4 chars
                lbsr    WriteLenY	; go write it	

                lda     2,S		; get ls 4 bits of trackno
                cmpa    #'F'		; sector 15 (of this group)?
                bne     L076F		; no do next

                lbsr    WriteEol	; otherwise move to next line	

L076F           leas    4,S		; clean up stack

L0771           ldd     <CurrentTrack	; get current track
                addd    #$0001		; add one to it
                std     <CurrentTrack	; store it back
                ldd     <SecPerTrk	; get sectors / track
                std     <GoodSecCounter	; assume all good
		
L077C           dec     <ClusterCount	; decement cluster count
                bne     L0792		; skip if not zero

                bsr     L07BB		

                tst     <u0029
                bne     L078C

                ldx     <GoodSectors	; get good sector count
                leax    1,X		; add 1 to it
                stx     <GoodSectors	; save it back
		
L078C           clr     <u0029
                lda     <SegAllocSize
                sta     <ClusterCount
		
L0792           ldb     <CurrentLSN	; add 1 to current LSN
                ldx     <CurrentLSN+1
                leax    1,X		; increment lower 16 bits
                bne     L079B		; no carry skip

                incb			; carry to top byte
		
L079B           cmpb    <TotalSectors	; compare to total sector count
                blo     L07A3		; lower skip

                cmpx    <TotalSectors+1	; compare lower 16 bits
                bhs     L07AA		; higher skip

L07A3           stb     <CurrentLSN	; update current LSN
                stx     <CurrentLSN+1
                lbra    L0705		; loop again

; come here when CurrentLSN > TotalSectors
L07AA           lda     #$FF		
                sta     <u0029
                leay    OptBuf,U
L07B2           cmpy    <u0037
                beq     WriteGoodSecs	; write total number of good sectors to screen

                bsr     L07BB

                bra     L07B2

L07BB           ldx     <u0037
                lda     <u0029
                rora
                rol     ,X+
                inc     <u003B
                lda     <u003B
                cmpa    #$08
                blo     L07DD

                clr     <u003B
                stx     <u0037
                cmpx    <u0039
                bne     L07DD

                bsr     L083A

                leax    OptBuf,U
                stx     <u0037
                lbsr    ClearBufX

L07DD           rts

; returns A converted to ASCII hex in D
HexDigits       tfr     A,B		; copy a to b
                lsra			; move ms 4 bits to ls
                lsra
                lsra
                lsra
                andb    #$0F		; mask out top bits
                addd    #'0'*256+'0'	; convert to ascii		
                cmpa    #'9'		; is a > 9
                bls     L07EF		; no check b

                adda    #$07		; adjust a
L07EF           cmpb    #'9'		; is b > 9 ?
                bls     L07F5		; no return

                addb    #$07		; adjust b
L07F5           rts

WriteGoodSecs   lbsr    WriteEol	; move to new line

                leax    NumGoodMsg,PCR	; point at good sectors message
                ldy     #NumGoodMsgL	; and length
                lbsr    WriteLenY	; go write it

                ldb     <SegAllocSize	; get cluster size
                clra
                ldx     <GoodSectors	; get number of good sectors
                pshs    X,A		; save count on stack

L080B           lsrb
                blo     L0816

                lsl     2,S
                rol     1,S
                rol     ,S
                bra     L080B

L0816           puls    X,A		; get counted sectors
                ldb     #C$CR		; eol
                pshs    B		; save it
	
                tfr     D,Y		; get size
                tfr     X,D
                tfr     B,A
                bsr     HexDigits	; get LS two didgits of size

                pshs    B,A		; stack them
                tfr     X,D
                bsr     HexDigits	; get middle two digits

                pshs    B,A		; stack them
                tfr     Y,D
                bsr     HexDigits	; get MS two digits

                pshs    B,A		; stack them
                tfr     S,X		; point X at stacked characters
                lbsr    Write80		; and write them to screen

                leas    7,S		; clean up stack
                rts			; return
		
L083A           pshs    Y
                clra
                ldb     #$01
                cmpd    <u0033
                bne     L0855

                leax    OptBuf,U
                clra
                ldb     <NoSysClusters
                tfr     D,Y
                clrb
                os9     F$AllBit
                lblo    BadSysExit

L0855           lbsr    L08E9

                leax    OptBuf,U
                lbsr    WriteSec

                ldd     <TotalSectors
                cmpd    <CurrentLSN
                blo     L0871

                bhi     L086E

                ldb     <TotalSectors+2
                cmpb    <CurrentLSN+2
                bhs     L0871

L086E           lbsr    GetNextSec

L0871           ldd     <u0033
                addd    #$0001
                std     <u0033
                puls    PC,Y            ; Pull of PC, effective RTS

MakeRoot        bsr     L08E9

                leax    u02B0,U
                bsr     ClearBufX

                leax    u02B3,U
                os9     F$Time
                leax    u02B0,U
                lda     #$BF
                sta     ,X
                lda     #$02
                sta     8,X
                clra
                ldb     #$40
                std     11,X
                ldb     <RootSectors
                decb
                stb     <20,X
                ldd     <u0033
                addd    #$0001
                std     <17,X
                bsr     WriteSec

                bsr     ClearBuf

                ldd     #$2EAE
                std     ,X
                stb     <32,X
                ldd     <u0033
                std     <30,X
                std     <62,X
                bsr     WriteSec

                bsr     ClearBuf

                ldb     <RootSectors
                decb
L08C3           decb
                bne     L08C7

                rts
L08C7           pshs    B
                bsr     WriteSec

                puls    B
                bra     L08C3

ClearBuf        leax    Buffer,U	; point to buffer
ClearBufX       clra			; d=0	
                clrb
L08D5           sta     D,X		; save in buffer
                decb			; decrement count
                bne     L08D5		; loop if more
                rts
		
WriteSec        lda     <DevFID		; get device file id
                ldy     #$0100		; one sector's worth
                os9     I$Write		; write it
                lblo    Exit		; exit on error

                rts			
		
L08E9           clra
                ldb     <u0033
                tfr     D,X
                lda     <u0034
                clrb
                tfr     D,U
L08F3           lda     <DevFID
                os9     I$Seek
                ldu     <Statics
                lblo    Exit

                rts

GetNextSec      ldx     <CurrentLSN
                lda     <CurrentLSN+2
                clrb
                addd    #$0100
                tfr     D,U
                bhs     L08F3

                leax    1,X
                bra     L08F3

L090F           lbsr    WriteEol

                leax    TableMsg,PCR
                lbsr    Write80

                lbsr    WriteEol

                tst     <PDDiskType
                bmi     L0954

                leax    RecordMsg,PCR
                ldy     #RecordMsgL
                lbsr    WriteLenY

                leax    FMMsg,PCR
                ldb     <IsMFM
                beq     L0937

                leax    MFMMsg,PCR
L0937           lbsr    Write80

                leax    DensityTPIMsg,PCR
                ldy     #DensityTPIMsgL
                lbsr    WriteLenY

                leax    TPI48Msg,PCR
                ldb     <IsDoubleTrk
                beq     L0951

                leax    TPI96Msg,PCR
L0951           lbsr    Write80

L0954           leax    NumCylMsg,PCR
                ldy     #NumCylMsgL
                lbsr    WriteLenY

                ldd     <NoTracks
                lbsr    L09EA

                leax    NumHeadMsg,PCR
                ldy     #NumHeadMsgL
                lbsr    WriteLenY

                clra
                ldb     <Sides
                bsr     L09EA

                leax    InterleavMsg,PCR
                ldy     #InterleavMsgL
                lbsr    WriteLenY

                clra
                ldb     <Interleave
                bsr     L09EA

                lbsr    WriteEol

                lbsr    WriteEol

                leax    DiskTypeMsg,PCR
                ldy     #DiskTypeMsgL
                lbsr    WriteLenY

                leax    Buffer,U
                ldb     <PDDiskType
                bmi     L09B5

                lda     #$38
                bitb    #$01
                bne     L09A5

                lda     #$35
L09A5           sta     3,X
                lda     #$20
                sta     ,X
                sta     1,X
                sta     2,X
                lda     #$0D
                sta     4,X
                bra     L09B9

L09B5           leax    HardMsg,PCR
L09B9           lbsr    Write80

                tst     <PDDiskType
                bmi     L09D4

                tst     <DgnCoCo
                bne     L09D4

                leax    SecTrk0Msg,PCR
                ldy     #SecTrk0MsgL
                lbsr    WriteLenY

                leay    <SecPerTrk0,U
                bsr     L09E8

L09D4           leax    SecTrkMsg,PCR
                ldy     #SecTrkMsgL
                lbsr    WriteLenY

                leay    <SecPerTrk,U
                bsr     L09E8

                lbsr    WriteEol

                rts
L09E8           ldd     ,Y
L09EA           leau    Buffer,U
                leax    L0A30,PCR
                ldy     #$2F20
L09F6           leay    256,Y
                subd    ,X
                bhs     L09F6

                addd    ,X++
                pshs    B,A
                ldd     ,X
                tfr     Y,D
                beq     L0A1E

                ldy     #$2F30
                cmpd    #$3020
                bne     L0A18

                ldy     #$2F20
                tfr     B,A
L0A18           sta     ,U+
                puls    B,A
                bra     L09F6

L0A1E           sta     ,U+
                lda     #$0D
                sta     ,U
                ldu     <Statics
                leas    2,S
                leax    Buffer,U
                lbsr    Write80

                rts
L0A30           fdb	10000,1000,100,10,1,0

ReadWord        ldd     #0		; clear total
L0A3F           bsr     L0A4F		; add next digit to total
L0A41           blo     L0A49		; overflow?, yep set conv=1
                bne     L0A3F		; if conversion not finished get next didgit

                std     <WordConv	; save accumulated value
                bne     L0A4E		; no error return

L0A49           ldd     #1		; set read word to 1
                std     <WordConv
L0A4E           rts

L0A4F           pshs    Y,D		; save regs
                ldb     ,X+		; get a byte from command line
                subb    #'0'		; make it zero based.....
                cmpb    #$0A		; out of range?
                bhs     L0A6D		; yep, end of conversion

                lda     #$00		; zero msb of word
                ldy     #$a		; 10 itterations
L0A5F           addd    ,S		; add current digit to saved D on stack
                blo     L0A6B		

                leay    -1,Y		; decrement counter
                bne     L0A5F		; loop again

                std     ,S		; save back on stack
                andcc   #~Zero		; clear zero flag
L0A6B           puls    PC,Y,D          ; restore and return

L0A6D           orcc    #Zero		; set zero flag
                puls    PC,Y,B,A        ; restore and return

PrintErrorExit  lda     #$02		; standard error
                os9     F$PErr		; print it
		
                leax    <HelpMsg,PCR	; point at help message
                ldy     #HelpMsgL+$4C	; get length
                lda     #$02		; standard error
                os9     I$WritLn	; go write it	
                clrb			; clear error
                os9     F$Exit		; exit
				
TitleMsg        FCB     C$LF
		FCC     "           OS-9 Disk Format Utility"

L0AAA           FCB     C$CR

HelpMsg         FCC     "Use: FORMAT /devname <opts>"
		FCB     C$LF
		FCC     "  opts: S/D - density; single or double"
		FCB     C$LF
		FCC     "        R   - Ready"
		FCB     C$LF
		FCC     /        "disk name"/
		FCB     C$LF
		FCC     "        1/2 - number of sides"
		FCB     C$LF
		FCC     "        'No. of tracks'      (in decimal)"
		FCB     C$LF
		FCC     "        :Interleave value:   (in decimal)"
		FCB     C$LF
		FCC     "        /Cluster size/       (in decimal)"
		FCB     C$CR
HelpMsgL	equ	*-HelpMsg

FormatMsg       FCC     "Formatting drive "
FormatMsgL	equ	*-FormatMsg

ReadyMsg        FCC     "y (yes), n (no), or q (quit)"
		FCB     C$LF
		FCC     "Ready?  "
ReadyMsgL	equ	*-ReadyMsg

AbortIntMsg     FCC     "ABORT Interleave value out of range"
		FCB     C$CR

DiskNameMsg     FCC     "Disk name: "
DiskNameMsgL	equ	*-DiskNameMsg

HowManyCylMsg   FCC     "How many Cylinders (Tracks?) : "
HowManyCylMsgL	equ	*-HowManyCylMsg

BadSysSecMsg    FCC     "Bad system sector, "

FormatAbtMsg    FCC     "FORMAT ABORT"
		FCB     C$CR

CluSizeMisMsg   FCC     "Cluster size mismatch"
		FCB     C$CR

DoubleDenMsg    FCC     "Double density? "
DoubleDenMsgL	equ	*-DoubleDenMsg

ChangeTPIMsg    FCC     "Change from 96tpi to 48tpi? "
ChangeTPIMsgL	equ	*-ChangeTPIMsg

DoubleSideMsg	FCC     "Double sided? "
DoubleSideMsgL	equ	*-DoubleSideMsg

NumGoodMsg	FCC     "Number of good sectors: $"
NumGoodMsgL	equ	*-NumGoodMsg

TableMsg        FCC     "            TABLE OF FORMAT VARIABLES"
		FCB     C$CR

RecordMsg       FCC     "              Recording Format: "
RecordMsgL	equ	*-RecordMsg

FMMsg           FCC     "   FM"
		FCB     C$CR

MFMMsg          FCC     "  MFM"
		FCB     C$CR

DensityTPIMsg	FCC     "          Track density in TPI:    "
DensityTPIMsgL	equ	*-DensityTPIMsg

TPI48Msg        FCC     "48"
		FCB     C$CR

TPI96Msg        FCC     "96"
		FCB	C$CR

NumCylMsg	FCC     "           Number of Cylinders: "
NumCylMsgL	equ	*-NumCylMsg

NumHeadMsg	FCC     "            Number of Surfaces: "
NumHeadMsgL	equ	*-NumHeadMsg

InterleavMsg	FCC     "      Sector Interleave Offset: "
InterleavMsgL	equ	*-InterleavMsg

DiskTypeMsg	FCC     "                     Disk type:  "
DiskTypeMsgL	equ	*-DiskTypeMsg

HardMsg         FCC     "HARD"
		FCB     C$CR

SecTrk0Msg	FCC     "Sectors/Track on TRK 0, Side 0: "
SecTrk0MsgL	equ	*-SecTrk0Msg

SecTrkMsg	FCC     "                 Sectors/Track: "
SecTrkMsgL	equ	*-SecTrkMsg

PhyLogMsg       FCC     "Both PHYSICAL and LOGICAL format? "
PhyLogMsgL	equ	*-PhyLogMsg

PhyVerifyMsg	FCC     "Physical Verify desired? "
PhyVerifyMsgL	equ	*-PhyVerifyMsg

                emod
eom             equ     *
                end

