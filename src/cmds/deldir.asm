                nam     Deldir
                ttl     program         ; module

* Disassembled 1900/00/00 00:42:37 by Disasm v1.5 (C) 1988 by RML

                
                use     defsfile
                
tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     $01
                mod     eom,name,tylg,atrv,start,size

BUFFLEN		equ	80		; buffer length

FlagQuit	equ	$01		; flag quit
FlagList	equ	$02		; flag list dir
FlagDelete	equ	$04		; flag delete dir

ParamPtr        rmb     2		; parameter pointer
PathFID         rmb     1		; dorectory path file ID
Flags           rmb     1		; flags
PathOpts        rmb     32
DirEntBuf       rmb     DIR.SZ		; directory entry buffer
u0044		rmb	2
NameLen         rmb     2		; file name len
EntNameLen      rmb     2		; length of name in DirEntBuf
Buffer          rmb     336
size            equ     .

name            equ     *
                fcs     /Deldir/
                fcb     $03

start           equ     *
L0014           bsr     OpenPath	; open specified path
                bcs     OpenError	; Open error, exit
		
                bsr     GetOpts		
                bcc     L002B		; no error, close path id and exit
		
                lbsr    PromptUser	; prompt user what they want to do
                bcs     OpenError	; quit or error, quit
                
		lbsr    DelThisDir	; delete this dir (and below)
                bcs     OpenError	; error quit
                
		lbsr    L0242
                bcs     OpenError	; error quit
		
L002B           lda     <PathFID	; get path file id
                os9     I$Close		; close it
                bcs     Exit		; exit on error
		
                ldx     <ParamPtr	; point to parameter
                os9     I$Delete	; delete it
                bcs     Exit		; error, exit 
		
                lda     ,x		; get next char			
                cmpa    #C$CR		; CR ?
                bne     L0014		; nope, keep going
                clrb			; flag no error
                bra     Exit		; exit
		
OpenError       pshs    b		; save error code
                lda     <PathFID	; get file id of path
                os9     I$Close		; close it
                puls    b		; restore error code
                orcc    #Carry		; flag error
Exit           	os9     F$Exit		; exit

OpenPath        stx     <ParamPtr	; save parameter pointer
                lda     #UPDAT.		; open for update
                os9     I$Open		; open directory path
                bcs     L005D		; error, skip
		
                sta     <PathFID	; save path file id
                bra     L0089		; skip on
		
L005D           ldx     <ParamPtr	; point to parameters 
                lda     #DIR.+READ.	; try open as directory
                os9     I$Open		; open it
                bcs     L0090		; error, skip
		
                sta     <PathFID	; save path file id
		
L0068           ldx     <ParamPtr	; point to parameters 
                os9     F$PrsNam	; parse the name
		
                clra			; length of name +1
                incb
                std     <NameLen	; save name length
		
                lda     ,y		; get character at end of name
                cmpa    #'/'		; slash?
                bne     L0089		; nope, skip
		
                lda     #C$CR		; make it a line feed
                sta     ,y+		; save it
		
                lda     #UPDAT.		; access mode
                ldx     <ParamPtr	; point at parameters
                os9     I$ChgDir	; change directory (to the root of the device)
		
                bcs     L0090		; error...skip
                
		sty     <ParamPtr	; pointer to rest of path.....
                bra     L0068		; loop again if more dirs.....
		
L0089           leax    <-$1C,u		; not sure what this does, comes here if file open succeeds
                stx     <u0044
                clr     <Flags
L0090           rts			; return

GetOpts         lda     <PathFID	; get path file id
                ldb     #SS.Opt		; get optios area of it's path descriptor
                leax    PathOpts,u	; point at buffer
                os9     I$GetStt	; go get them
                bcs     L00AB		; error, return
                
		ldx     <u0044
                lda     <$33,x
                anda    #$80
                beq     L00AA		; no error
		
                clrb			; clear b
                orcc    #Carry		; flag error
                bra     L00AB		; skip clear
L00AA           clrb
L00AB           rts

Prompt   	fcb   	C$LF
		fcc   	"Deleting directory file. "
		fcb   	C$LF
		fcc   	"List directory, delete directory, or quit ? (l/d/q) "
PromptL  	equ   	*-Prompt

Continue     	fcb   	C$LF
		fcc   	"Continue? (y/n) "
ContinueL    	equ   	*-Continue				

PromptUser      tstb			; should we prompt?
                bne     L013E		; nope, skip to return
		
                lda     #$01		; standard out
                leax    <Prompt,pcr	; point at prompt
                ldy     #PromptL	; length of prompt
                os9     I$WritLn	; write it

L011B           bcs     L013E		; error, return
                bsr     ReadKey		; read keys from user
                bcs     L013E		; erro, return
		
                ldb     <Flags		; get the flags
                cmpb    #FlagQuit	; should we quit?
                bne     L012A		; no....skip
                clrb			; flag no error
                bra     L013E		; and return
		
L012A           bsr     DoDir		; do a directory
L012C           bcs     L013E		; error, exit

                leax    <Continue,pcr	; point at continue message
                ldy     #ContinueL	; get it's length
                lda     #$01		; standard out
                os9     I$WritLn	; write it	
                bcs     L013E		; error, exit
		
                bsr     ReadKey		; read a key
L013E           rts

Dir      	fcc   	"DIR"		; dir command
		fcb   	C$CR
DirOpts  	fcc   	"E "		; options for dir command
DirOptsL  	equ   	*-DirOpts
		
; write dir command into buffer and execute it		
DoDir           pshs    u		; save u
		leau    <Buffer,u	; point to buffer
		pshs    u		; save u
		
                ldb     #DirOptsL	; length of dir opts
                leax    <DirOpts,pcr	; point to diropts
                lbsr    CopyBXtoU	; copy them
		
                ldx     <ParamPtr	; point at parameters
                ldd     <NameLen	; get name length
                decb			; -1
                lbsr    CopyBXtoU	; copy it
		
                lda     #C$CR		; terminate command string
L015E           sta     ,u+
                tfr     u,d		; put end of buffer pointer in d
                subd    ,s		; subtract beginning of buffer (on stack)
                tfr     d,y		; y now contains the size of param area for dir
                puls    u		; restore address of params
                leax    <Dir,pcr	; point to module to run
                lda     #Prgrm+Objct	; type of module
                clrb			; no extra data pages
                os9     F$Fork		; go run it
		
                puls    u		; restore u
                bcs     L013E		; exit on error
		
                os9     F$Wait		; wait for process to complete
L0178           rts			; return

; Note read key scans for the keys Y, L and D, but the prompt instructs 
; the user to press l,d or q, though this still works as q is treated as
; unrecognised key and aborts anyway!

ReadKey         leax    <Buffer,u	; point at buffer
                ldy     #BUFFLEN	; length of buffer
                lda     #0		; standard in
                os9     I$ReadLn	; go read.....
                bcs     L01B8		; error, skip to return
		
L0187           lda     ,x+		; get a byte from buffer
                cmpa    #' '		; space?		
                beq     L0187		; yep, skip over spaces
		
                eora    #'Y'		; is it Y for quit?
                anda    #$DF		; mask upper
                beq     L01AD		; yes flag it
		
                lda     ,-x		; restore from buffer
                eora    #'L'		; is it L for list?
                anda    #$DF		; mask upper
                beq     L01A9		; yes flag it
		
                lda     ,x		; restore from buffer
                eora    #'D'		; is it D for dir?
                anda    #$DF		; mask upper
                beq     L01A5		; yes flag it
		
                bra     L01B4		; none of the above, assume Q
		
L01A5           ldb     #FlagQuit	; flag as quit
                bra     L01AF		; store and return
		
L01A9           ldb     #FlagList	; flag as list
                bra     L01AF		; store and return
		
L01AD           ldb     #FlagDelete	; flag as delete
L01AF           stb     <Flags		; save flags
                clrb			; no error
                bra     L01B8		; jump to return
		
L01B4           ldb     #$01
                orcc    #Carry		; flag error
L01B8           rts

DelDir   	fcc   	"DELDIR"
		fcb   	C$CR

DotDot   	fcc   	".."
		fcb   	C$CR

DelThisDir      ldb     <Flags		; get flags
                bitb    #FlagQuit+FlagDelete	; test flags
                beq     L0210		
		
                lda     <PathFID	; get path file id
                pshs    u		; save u

; read the directory....		
                ldu     #DIR.SZ*2	; Seek to : $0000:$0040
L01D0           ldx     #$0000		; so past . and .. entries....
                os9     I$Seek		; go do seek
                puls    u		; recover u
		
L01D8           bsr     GetNextEnt	; get next directory entry	
                bcs     L0209		; error exit
		
                ldx     <ParamPtr	; point to parameters
                lda     #UPDAT.		; access mode, update, so change working dir
                os9     I$ChgDir	; go change it
                bcs     L0214		; error, exit
		
                ldy     <EntNameLen	; get entry name length
                clrb			
                lda     #Prgrm+Objct	; type of module
                pshs    u		; save u
                leau    <DirEntBuf,u	; point to dir entry buffer, parameters for deldir
                leax    <DelDir,pcr	; point to program to run
                os9     F$Fork		; go run it
                
		puls    u		; restore u
                bcs     L0214		; on error (from fork), exit
		
                os9     F$Wait		; wait for forked deldir to terminate
                bcs     L0214		; error, exit
		
                leax    <DotDot,pcr	; point to path, to change to parent dir
                lda     #UPDAT.		; access mode
                os9     I$ChgDir	; go change it
                bcc     L01D8		; no error, so do next entry 

L0209           cmpb    #E$EOF		; end of file error?
                bne     L0214		; no return it to caller
                clrb			; no error if eof
                bra     L0214		; return to caller
		
L0210           ldb     #$01		; flag quit
                orcc    #Carry		; flag error
L0214           rts

GetNextEnt      lda     <PathFID	; get path file id	
                leax    <DirEntBuf,u	; point at buffer
                ldy     #DIR.SZ		; dir entry size
                os9     I$Read		; read into buffer
                bcs     L0238		; exit on error
		
                lda     ,x		; get first char of buffer
                beq     GetNextEnt	; zero, null entry, skip and read next
		
                os9     F$PrsNam	; otherwise parse the name
                lda     -$01,y		; get last byte of path before '/'
                anda    #$7F		; clear ms bit
                sta     -$01,y		; store it back
                lda     #C$CR		; and terminate with eol
                sta     ,y		; store it 
                clra			; convert name len to 16 bit
                incb			; add one
                std     <EntNameLen	; save it
L0238           rts

Attr     	fcc   	"ATTR"		; attribute command
		fcb   	C$CR

AttrOpts 	fcc   	" -d"		; attribute options
		fcb   	C$CR
AttrOptsL 	equ   	*-AttrOpts

L0242           pshs  	u		; save u
		leau  	<buffer,u	; point to buffer
		pshs    u		; save buffer address
                ldd     <NameLen	; get name length
                decb			; -1
                ldx     <ParamPtr	; point to parameters
                bsr     CopyBXtoU	; copy name
		
                leax    <AttrOpts,pcr	; point to attribute options
                ldb     #AttrOptsL	; attribute options length
                bsr     CopyBXtoU	; copy it to buffer
		
                tfr     u,d		; copy buffer end pointer to d
                subd    ,s		; subtract buffer start pointer (on stack)
                tfr     d,y		; we now have buffer length in y
                
		puls    u		; restore buffer pointer
                leax    <Attr,pcr	; point to command to run
                clrb
                lda     #Prgrm+Objct	; type of module
                os9     F$Fork		; go run attr
                bcs     L026D		; on error, exit
		
                os9     F$Wait		; wait for attr to terminate
L026D           puls    u		; restore our u
                rts

CopyBXtoU       decb			; decrement count
                lda     ,x+		; get a byte from x
                sta     ,u+		; put byte in u	
                tstb			; done all?
                bne     CopyBXtoU	; nope loop again
                rts			; return
		
                emod
eom             equ     *
                end
