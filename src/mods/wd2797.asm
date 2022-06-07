                nam     WD2797          
                ttl     Floppy          ; disk driver with bootstrap
                                        
****************************************
*
* Floppy disc driver with bootstrap routine
* for the Western Digital 2797 disc controller.
*
* For the Dragon 128 computer.
*
* last mod 21/12/83
*
****************************************
                                        
****************************************
*
* The boot routine is an extension to
* OS-9 standard disc driver capabilities.
* The entry point is the 7th entry in the
* branch table, The bootstrap routine expects
* the Y register to contain the address of
* the Device Descriptor for the boot device.
*
* This driver can handle both mini and
* full-size drives, provided the appropiate
* bit is set in the Device Descriptor.
*
* This driver can handle discs formatted with
* 128 byte sectors. The first sector on each
* track must be sector 1. The Device
* Descriptor 'sectors per track' entries must
* contain the number of sectors per track
* divided by two, and bit 5 of the DD.TYP
* entry must be set, (this is an extension
* to the standard OS-9 definitions for this
* byte). Bit 6 should also be set, to indicate
* Non-OS9 Format.
*
*
* Written by Paul Dayan of Vivaway Ltd
*
* History:
* Written 3rd November 1983
*
****************************************
                                        
                use     defsfile        
                
                        
                ttl     Floppy          ; disk driver with bootstrap
                                        
Drvcnt          set     4               ; Four Drives
FULLSIZE        equ     1               ; include 8' drives
DDTRACK0        equ     1               ; single and double density on track 0
DBLTRACK        equ     1               ; single and double track density
TRYBOTH         equ     0               ; get track 0 density info from PD
EXTFMT          equ     1               ; multiple formats
HLFSECT         equ     1               ; 256 and 128 byte sectors
DBLSIDE         equ     1               ; single and double sided
STEPIN          equ     1               ; step in before restore
                                        

                ifeq    EXTFMT          
                endc                    

                                        
                                        
;                org     0               
;u0000           rmb     1               
;XV.Port         rmb     2                              
;u0003           rmb     1               
;u0004           rmb     1               
;XV.Wake         rmb     1               
		org	V.USER
u0006           rmb     2               
u0008           rmb     1               
V.DrvInUse      rmb     1               
u000A           rmb     2               
u000C           rmb     1               
u000D           rmb     1               
                                        
                org     Drvbeg          ; reserve RBF static storage
                rmb     Drvmem*Drvcnt   ; drive tables
V.Cdrv          rmb     2               ; address of current drive table
V.Vbuff         rmb     2               ; address of verify buffer
V.Wrtf          rmb     1               ; write type flag
V.Wait          rmb     1               ; 'can wait by sleeping' flag
V.Active        rmb     1               ; 'command in progress' flag
V.Status        rmb     1               ; FDC status register
V.T0stk         rmb     2               ; sectors on track 0
V.Stk           rmb     2               ; sectors per track
V.Drive         rmb     2               ; current drive number
V.Lsn           rmb     2               ; logical sector number
V.Fmt           rmb     1               ; drive type and disk format
V.TwoStp        rmb     1               ; double stepping flag
V.Timer         rmb     1               ; motor off timer
V.Cmd           rmb     1               ; command code
V.Sector        rmb     1               ; sector number
V.CrTrk         rmb     1               ; current track
V.Buff          rmb     2               ; address of read/write buffer
V.Bytc          rmb     2               ; number of bytes to transfer
V.Step          rmb     1               ; step rate in ms
V.Track         rmb     1               ; required track
V.Select        rmb     1               ; current select register
V.NewTrk        rmb     1               ; 'new track' flag

                ifne    DBLSIDE         
V.Side          rmb     1               ; required head
                endc                    

V.Freeze        rmb     1               ; freeze drive table flag

                ifne    DDTRACK0        
V.DDTr0         rmb     1               ; double density on track 0 flag
                endc                    

                                        

                ifne    DBLSIDE         
Dblsid          set     1               ; double sided bit
                endc                    


                ifne    EXTFMT          
Dblden          set     2               ; double density bit
                endc                    


                ifne    DBLTRACK        
Dbltrk          set     4               
                endc                    


                ifne    HLFSECT         
Hlfsec          set     8               
                endc                    


                ifne    DDTRACK0        
DDTr0           set     16              ; double density on track 0
                endc                    

                                        
                                        
Dskmem          equ     .               
Btmem           equ     Dskmem-V.Wrtf   ; Memory for bootstrap
                                        
*
* Timings
*
MotTim          equ     250             ; 5 second motor on
RdyTim          equ     12              ; 1.2 sec pause after motor on
TickOut         equ     200             ; abort timeout
*
* Precompensation track
*
PrcTrk40        equ     16              ; for 40 track

                ifne    DBLTRACK        
PrcTrk80        equ     40  		; for 80 track            
                endc                    

*
* Flag defines for V.Fmt
*
VFmt.Dblsid	equ	%00000001	; Double sided disk
VFmt.MFM	equ	%00000010	; Double density disk
VFmt.HlfSec	equ	%00001000	; 128 byte sectors
VFmt.Full	equ	%10000000	; fullsize (5.25")	

*
* Driver specific PD.TYP bits.
*       
TYP.128		equ	%00001000	; 128 byte sectors 
                                 
Type            set     drivr+objct     
Revs            set     reent+1         
                                        
                                        
                mod     Dkend,Dknam,Type,Revs,Dkent,Dskmem 
                fcb     $FF             
                                        
Dknam           fcs     'wd2797'        
                fcb     1               ; Edition
                                        
*Entry Table
Dkent           lbra    Idisk           
                lbra    Read            
                lbra    Write           
                lbra    Gstat           
                lbra    Pstat           
                lbra    Term            
                lbra    Boot            

                                        
Fdcpol          fcb     0,INTBIT,128    
                                        
Maxcyl          equ     40              
Scttrk          equ     18              
Trkcyl          equ     1               
Sctcyl          equ     Scttrk*Trkcyl   
Maxsct          equ     (Maxcyl*Sctcyl)-1 
                                        
* Initialise Controller And Storage
* Input: Y=Device Descriptor Pointer
*        U=Global Storage Pointer
                                        
Idisk           
		lda     #Drvcnt         ; No. Of Drives
                sta     V.Ndrv,u        
                leax    Drvbeg,u        ; Get D0 Table Pointer
                stx     V.Cdrv,u        ; Set In Current Drive Pointer
                pshs    U               ; Save Static Storage Pointer
                leau    V.Wrtf,u        ; Adjust Static For Boot
                lbsr    Getdd           ; Grab Parameters From Device Descriptor

                ldd     #$100           ; Get Verify Buffer
                os9     F$SRqMem        ; Get Memory

                bcs     Ierr            ; Skip If Error

                tfr     u,d             ; Copy Buffer Pointer
                puls    u               ; Retrieve Static Storage Pointer
                std     V.Vbuff,u       ; Save Verify Buffer Pointer
                ldd     #INTPIA+1       ; point at PIA control reg
                leax    <Fdcpol,pcr     ; Get Polling Parameters
                leay    Fdcsrv,pcr      ; Get Service Routine
                os9     F$IRQ           ; Try For Polling Table

                bcs     Init30          ; Skip If Error

                inc     V.Wait,u        ; Can Now Wait By Sleeping
                ldx     #INTPIA         ; point at interrupt PIA
                lda     ,x              ; clear any present interrupt
                pshs    cc              ; save masks
                orcc    #IntMasks       ; mask interrupts
                lda     1,x             ; get control reg
* anda #$C1 set CB2 interrupt input
                ora     #3              ; positive edge on CA1; enable /INTRQ interrupt
* ora #$18 positive edge
                sta     1,x             
                puls    cc              ; restore masks
                leax    TimSrv,pcr      ; install timer routine
                os9     F$Timer         

                bcs     Init30          ; ..error

Init10          leax    Drvbeg,u        ; Get DO Table Pointer
                ldb     #DRVCNT         
                lda     #$FF            
Init20          sta     DD.TOT+1,x      ; non-zero size
                sta     V.TRAK,x        ; crazy track
                leax    DRVMEM,X        ; next!
                decb                    
                bne     Init20          

                clrb    No              ; Error
Init30          rts                     
                                        
Ierr            puls    U,Pc            ; Abort
                                        
                pag                     
*************************************************************
*
* Read Sector Subroutine
* Input: B=Logical Sector Msb
*        X=Logical Sector Lsbs
*        Y=Path Descriptor
*        U=Global Storage
*
Read            bsr     Rngtst          ; Check Lsn Range

                bcs     Init30          ; ..error

                ldx     V.Lsn,u         ; sector 0?
                bne     Read1           ; ..no

                bsr     Read1           ; read sector

                bcs     Init30          ; ..error

                ldx     PD.BUF,Y        ; Get Buffer Pointer
                pshs    y,x             ; Save Registers
                tst     V.Freeze,u      ; freeze info?
                bne     Read2           ; ..yes; skip copy

                ldy     V.Cdrv,u        ; Get Drive Table Pointer
                ldb     #Dd.Siz-1       ; Get Buffer Offset/Counter
Copytb          lda     b,x             ; Get Buffer Byte
                sta     b,y             ; Set It In Table
                decb                    ; Count Bytes
                bpl     Copytb          ; Loop Until Done


                ifne    DDTRACK0&TRYBOTH 
                endc                    

Read2           clr     V.Freeze,u      ; clear carry; no more freeze
                puls    pc,y,x          ; Return
Read1           leax    Rsect,pcr       ; Point At Read Routine
                                        
* Fall Through To Call Controller Subroutine
                                        
* Call Controller Subroutine
* Get Controller Pointer, Adjust Static Storage
* Pointer, And Call Routine
                                        
* Input: U=Static Storage Pointer
                                        
Call            pshs    u,y             ; Save Registers
                ldy     V.Port,u        ; Get Port Address
                leau    V.Wrtf,u        ; Move Static Storage Pointer
                jsr     ,x              ; Call Driver

                puls    pc,u,y          
                                        
* Write Sector
                                        
* Input: B=Lsn Msb
*        X=Lsn Lsbs
*        Y=Path Descriptor Pointer
*        U=Global Storage
                                        
Write           bsr     Rngtst          ; Check Lsn Range

                bcs     Return          ; Skip If Error

                leax    Wsect,pcr       ; Get Write Routine Pointer
                bsr     Call            ; Call Routine

                bcs     Return          ; Skip If Error

                lda     Pd.Vfy,y        ; Verify On?
                bne     Return          ; Skip If Not

                ldd     PD.BUF,y        ; saver buffer address
                pshs    D               
                ldd     V.Vbuff,u       ; Point At Verify Buffer
                std     PD.BUF,y        ; set verify buffer
                bsr     Read            ; do verify read

                puls    X               ; restore buffer address
                stx     PD.BUF,Y        
                rts                     
                                        
* Logical Sector Number Range Test
*
* Checks Logical Sector Number, Sets Buffer Pointer,
* Sectors/Track On Track 0, And Transfers The Information
* From The Path Descriptor For Non-Os9 Disks.
*
* LSN 24 bits in b:x
                                        
Rngtst          tstb    				; Lsn Possibly In Range?
                bne     Rngerr          ; Skip If No, LSN > 65535, so invalid for floppy

                stx     V.Lsn,u         ; save bottom 16 bits of LSN
                bsr     Getdrv          ; go get drive table pointer. 

                ifne    HLFSECT         
                bitb    #TYP.NSF        ; Non-Os9 Format?
                beq     Rng4   		; no skip on       

                anda    #$F8            ; mask format
                pshs    a               ; save it
                lda     PD.SID,y     	; get number of surfaces into a   
                deca                    ; decrement, so single sided=0, double=1
                puls    a               ; restore format byte, flags untouched
                beq     Rng1            ; branch if single sided

                ora     #VFmt.Dblsid        ; flag double sided
		
Rng1            ldb     PD.DNS,y        ; get density
                bitb    #DNS.MFM	; mfm disk ?             
                beq     Rng2            ; nope, skip

                ora     #VFmt.MFM       ; flag MFM
		
Rng2            sta     >V.Fmt,u        ; save format byte
                ldd     PD.SCT,y        ; get sectors / track
                std     V.Stk,u         ; save in working store

                ifne    DBLSIDE         
                lda     PD.SID,y        ; Do a rough total double sided
                deca                    ; make sizes zero based
                beq     Rng3            ; branch if single sided

                lslb                    ; multiply sec / track by 2 (as 2 sidees)
                endc                    

Rng3            lda     Pd.Cyl+1,y      ; Cylinders
                mul     		; Make Rough Sector Total
                std     Dd.Tot+1,x      ; Set It
                bra     Rng5            

                endc                    

Rng4            equ     *               

                ifne    EXTFMT          
                clra                    
                ldb     Dd.Tks,x        ; Set Sectors/Track
                std     V.Stk,u         
                endc                    

Rng5            ldd     V.Lsn,u         ; Get Lsn
                cmpd    Dd.Tot+1,x      ; less than total sectors on disk?
                bhi     Rngerr          ; nope : range error

                ldd     Pd.Buf,y        ; get buffer pointer
                std     V.Buff,u        ; save it in working store

                ifne    EXTFMT          
                ldd     PD.T0S,y        ; Get Track 0 Sectors/Track
                std     V.T0stk,u       ; Pass It
                endc                    

                clrb                  	; No Error
                rts                     
                                        
Rngerr          comb                    ; Indicate Error
                ldb     #E$SECT         ; Lsn Out Of Range
Return          rts                     
                                        
* Getdrv Drive Table Pointer
*
* Returns - A : Format
*           B : Type
*           X : Drive Table Pointer
                                        
Getdrv          ldx     V.Cdrv,u		; get current drive table pointer        
                beq     Get5            ; no table?

                lda     V.CrTrk,u  		; get current track     
                sta     V.TRAK,x        ; save in drive table
				
Get5            lda     PD.DRV,y        ; get drive number
                sta     V.Drive,u       ; store in current drive
                ldb     #DRVMEM   		; bytes / drive table entry      
                mul                     ; multiply, to find offset	
                leax    Drvbeg,u        ; point to beggining of drive table
                leax    d,x             ; add offset
				
                stx     >V.Cdrv,u       ; save current drive table pointer
                lda     <V.TRAK,x       ; get current track from drive table   
                sta     >V.CrTrk,u      ; save it in our working current track
				
                lda     PD.STP,y        ; get step rate from path descriptor
                anda    #STP.Mask       ; mask out invalid bits
                sta     >V.Step,u       ; save step rate in working storage	
                
		ldb     PD.DNS,y        ; get density from path descriptor
                andb    #DNS.DTD        ; determine if we should double step, 
                stb     >V.TwoStp,u     ; and store in working storage
				
                lda     <DD.FMT,x       ; get disk format from drive table   
                ldb     PD.TYP,y        ; get disk type from path descriptor
                andb    #TYP.CCF		; is it Dragon/CoCo format? (double density t0)            
                stb     >V.DDTr0,u      ; save in working storage
				
                ldb     PD.TYP,y        ; get disk type from path descriptor
                bitb    #TYP.NSF	; non standard format?            
                bne     Get10           ; yes....skip

                bita    #FMT.TDNS       ; Is it 48 or 96 TPI?
                beq     Get10           ; 48, skip

                clr     >V.TwoStp,u     ; don't double step
				
Get10           bitb    #TYP.3		; 3.5" media?            
                beq     Get15           ; nope, skip

                ora     #VFmt.Full	; set fullsize flag            
Get15           bitb    #TYP.128	; check for 128 byte sectors?            
                beq     Get20           ; nope, skip

                ora     #VFmt.HlfSec	; set 128 byte sector flag            
Get20           sta     >V.Fmt,u  	; save disk format flage      
                rts                     
                                        
* Get Status - Not Used
*
                                        
Gstat           bra     Pstat1          

                                        
                                        
* Put Status.
* Used For Restore And Write Track (Format)
*
* Input: Y=Path Descriptor Pointer
*        U=Static Storage
                                        
Pstat           pshs    U,Y             
                ldx     PD.RGS,Y        ; Point to parameters
                ldb     R$B,X           ; Get stat call
                cmpb    #SS.Reset       ; Restore?
                beq     Rstor           ; ..yes; do it.

                cmpb    #SS.WTrk        ; Write track call?
                beq     Wtrk            ; ..yes; do it.

                cmpb    #SS.FRZ         ; Freeze dd. info?
                beq     SetFrz          ; Yes; ....flag it.

                cmpb    #SS.SPT         ; Set sect/trk?
                beq     SetSpt          ; Yes; ....set it.

                puls    u,y             ; restore regs
Pstat1          comb                    ; flag error
                ldb     #E$UnkSvc	; unknown service            
                rts                     

* Freeze drive info for read of sector zero.
*
                                        
SetFrz          ldb     #$01            ; flag freeze
                stb     >V.Freeze,u     ; set it
                clrb                    ; no error
                puls    pc,u,y          ; restore and return

* Set sectors / track
*
                                        
SetSpt          ldx     R$X,x           ; get x from regs
                pshs    x               ; save it
                lbsr    Getdrv          ; get drive table pointer

                puls    d		; restore sec/track              
                std     DD.Tks,x    	; set new value    	   
                clrb                    ; no error
                puls    pc,u,y          ; restore and return

* Restore drive
*
                                        
Rstor           lbsr    Getdrv          ; set drive number

                ldx     >V.Cdrv,u       ; get drive table pointer
                clr     V.Trak,x    	; set track=0      
                leax    >Brstor,pcr     ; get address of restore routine in X
                lbsr    Call            ; go call it

                puls    pc,u,y          ; restore and return
                                        
* Write Track (Format)
*
TrkBufSize	equ	41*256		; size of track buffer
                                        
Wtrk            lda     R$Y+1,x 	; get format        
                ldb     R$U+1,x         ; get track
                pshs    d		; save them             
                lbsr    Getdrv          ; Get drive table pointer

                ifne    EXTFMT          
                ldd     Pd.Sct,y        ; get/set sectors per track
                std     V.Stk,u         
                ldd     Pd.T0s,y        ; get/set track 0 sectors / track
                std     V.T0stk,u       
                endc                    

                ldd     #0              ; fix logical sector to 0 to fool
                std     V.Lsn,u         
                puls    D               ; retrieve format and track bytes
                sta     Dd.Fmt,x        ; retore them
                stb     V.Track,u       

                ifne    DBLSIDE         
                anda    #VFmt.Dblsid	; is disk double sided ?              
                sta     V.Side,u        ; set it
                endc                    

                lbsr    Getdrv          ; calculate format....

                ldd     #TrkBufSize     ; get room for track buffer
                os9     F$SRqMem        ; request memory

                bcs     Wtrk8           ; error, no memory available

                ldx     D.Proc          ; get process ptr
                lda     P$Task,x        ; get source task
                ldb     D.SysTsk        ; get destination (system) task
                ldy     ,S              ; get PD ptr
                ldx     PD.RGS,y        ; get regs pointer
                ldx     R$X,x      	; point at format buffer      
                ldy     #TrkBufSize    	; size
                os9     F$Move          ; move it

                leax    ,u              ; copy buffer ptr
                ldu     2,s             ; retrieve static pointer
                stx     V.Buff,u        ; save buffer address
                
		leau    V.Wrtf,u        ; saves code.....
                ldy     V.Port-V.Wrtf,u ; get port address
                lbsr    Select          ; select drive

                bcs     Wtrk8           ; error : exit

                lbsr    Settrk          ; seek to track 

                ldd     #TrkBufSize	; exagerated byte count         
                std     V.Bytc-V.Wrtf,u  ; save it
		
                lda     #F.WRTR         ; command code for WD2797
                sta     V.Cmd-V.Wrtf,u  ; save it
                lda     #1              
                sta     V.Wrtf-V.Wrtf,u ; set write type operation 
                lbsr    IssXfr          ; do transfer

                pshs    b,cc            ; save error status
                ldu     V.Buff-V.Wrtf,u  ; get memory to return
                ldd     #TrkBufSize     ; size
                os9     F$SRtMem   	; free the memory     

                puls    b,cc            ; restore error status
Wtrk8           puls    pc,u,y          ; restore and return
                                        
* Terminate Device Usage
*
* Input: U=Static Storage
                                        
Term            pshs    u          	; save regs     
                ldu     V.Vbuff,u       ; get pointer to our buffer
                ldd     #$100           ; and buffer size
                os9     F$SRtMem        ; free memory

                puls    u               ; restore static storage ptr
                ldx     #INTPIA         ; turn off INT from PIA
                lda     1,x             ; get control register
                anda    #$FE            ; clear in enable bit
                sta     1,x             ; restore it
				
                ldx     #0              ; remove device from IRQ table
                os9     F$IRQ           

                ldx     #0              ; Remove timer routine
                os9     F$Timer         

                clrb                    ; no error
                rts                     
                                        
                                        
                                        
                                        
****************************************
*
* Bootstrap Routine, And Disk Controller
* Interface Routines
*
****************************************
                                        
Boot            pshs    u,y,x,b,a       
			
		leas    -Btmem,s        ; Get Global Storage
                ldd     #256            ; Get A Page
                os9     F$SRqMem        

                bcs     Bterr2          ; Skip If None

                stu     V.Buff-V.Wrtf,S ; Set Buffer Pointer
                leau    ,S              ; Set Storage Pointer
                clr     V.Wait-V.Wrtf,u ; can't sleep
Boot40          clra                    
                clrb                    
                std     V.Lsn-V.Wrtf,u  ; Logical Sector 0
                ldy     Btmem+4,S       ; Get Device Descriptor Pointer
                bsr     Getdd           ; Grab parameters

                lda     INTPIA+1        ; get control reg
                ora     #1              ; enable disk controller interrupts
                sta     INTPIA+1        
                lbsr    Brstor          ; Restore Drive

		ifndef	NORETRY
                bcs     Boot40          ; Skip If Error - Try Again
		else
		bcs	Bterr2		; Skip If Error - exit
		endc

                lbsr    Rsect           ; Read Sector 0

		ifndef	NORETRY
                bcs     Boot40          ; Skip If Error - Try Again
		else
		bcs	Bterr2		; Skip If Error - exit
		endc

		ldx     V.Buff-V.Wrtf,u ; Get Buffer Pointer

                ifne    EXTFMT          
                lda     Dd.Fmt,X        ; Get Disk Format

                ifne    FULLSIZE        
                ora     V.Fmt-V.Wrtf,u  ; Set Drive Type In Format
                sta     V.Fmt-V.Wrtf,u  ; Set It
                endc                    


                ifne    DBLTRACK        
                bita    #4              ; double track disk?
                beq     Boot20          ; ..no

                clr     V.TwoStp-V.Wrtf,u ; can't need double stepping
Boot20          clra                    
                ldb     Dd.Tks,x        ; Get Sects/Trk
                std     V.Stk-V.Wrtf,u  ; Set It
                endc                    

                ldd     Dd.Bsz,x        ; Get Boot File Size
                std     Btmem,u         
                ldd     #256            ; Return Page
                leau    ,x              
                ldx     DD.Bt+1,x       ; Get Start Sector Number
                os9     F$SRtMem        

                ldd     Btmem,S         ; was there a boot file?
                beq     Noboot          


                ifeq    LEVEL-1         
                os9     F$SRqMem        ; Get memory for bootstrap

                else                    
                os9     F$BtMem         ; Get memory for bootstrap

                endc                    

                bcs     Bterr2          ; Skip If None

                stu     Btmem+2,S       ; Return Address To Caller
                stu     V.Buff-V.Wrtf,s ; Set Buffer Pointer
                leau    ,S              ; Get Global Storage Pointer
Boot10          pshs    x,a             ; Save Pages And Sector Number
                stx     V.Lsn-V.Wrtf,u  ; Set Sector Number
                bsr     Rsect           ; Read Sector

                bcs     Bterr1          ; Skip If Error

                puls    x,a             ; Retrieve Pages And Sector Number
                leax    1,x             ; Next Sector
                inc     V.Buff-V.Wrtf,u ; Move Buffer Pointer
                deca                    ; Done All Pages
                bne     Boot10          ; Loop If Not

                leas    Btmem,s         ; Return Global Storage
                clrb    No              ; Error, Clear Carry
                puls    pc,u,y,x,b,a    ; Return, With Boot Address In D
Noboot          comb                    ; Set Carry
                ldb     #E$BTYP         ; No Boot File
                bra     Bterr2          

Bterr1          leas    3,s             ; Pitch Pages And Sector Number
Bterr2          leas    Btmem+2,S       ; Pitch Global And D
                puls    pc,u,y,x        
                                        
* Get Parameters From Device Descriptor
Getdd           leay    $12,y           ; adjust pointer why is this $12????
                lda     PD.Drv-PD.opt,y ; get drive number            
                sta     V.Drive-V.Wrtf,u  ; save it
		
                lda     PD.Stp-PD.Opt,y	; get step rate           
                anda    #STP.Mask	; make sure it's valid              
                sta     V.Step-V.Wrtf,u ; save it

                ifne    EXTFMT          
                lda     PD.Typ-PD.opt,y ; get drive type          
                clrb                    

                ifne    FULLSIZE        
                bita    #TYP.3		; 3.5" media?              
                beq     Getdd1          ; yes, skip

                ldb     #VFmt.Full	; flag fullsize (5.25")            
                endc                    

                                        
Getdd1          equ     *               

                ifne    DBLTRACK        
                lda     PD.dns-PD.opt,y		; get density flag           
                anda    #DNS.DTD	  	; double track?	              
                sta     V.TwoStp-V.Wrtf,u 	; yep set it
                endc                    


                ifne    DDTRACK0        

                ifeq    TRYBOTH         
                lda     (PD.dns-PD.opt)-1,y	; get density flag            
                anda    #DNS.T0DD		; double density on t0?            
                sta     V.DDTr0-V.Wrtf,u 	; save it
                endc                    

                endc                    

                stb     V.Fmt-V.Wrtf,u  	; save format byte
                ldd     PD.t0s-PD.opt,y      	; get sectors on track 0     
                std     V.T0stk-V.Wrtf,u 	; ; save it
                endc                    

                ldy     -$03,y 		; get port address in y         
                lda     STTREG,y        ; clear current interrupt
                rts                     
                                        
* Read Sector Routine
*
                                        
Rsect           clr     V.Wrtf-V.Wrtf,u ; clear	write type flag
                lda     #F.READ       	; Read sector command    
                sta     V.Cmd-V.Wrtf,u  ; save in currently execing command
                                        
* Set up for Read/Write Transfer
Xfr             lda     #$DB            
                pshs    a               
                lda     V.Fmt-V.Wrtf,u  ; get drive type / disk format
                bita    #VFmt.HlfSec	; 128 byte sectors ?            
                beq     Xfr1            ; nope, skip

* Since our buffer is 256 bytes, we read 2x128 byte sectors to fill the buffer.
*
                ldd     #128		; load 128 bytes at a time         
                std     V.Bytc-V.Wrtf,u  ; save it
                bsr     Xfr2            ; read the first sector (of two?)

                bcs     Xfr70           ; error : exit

                ldd     V.Buff-V.Wrtf,u ; bump the buffer pointer up
                leas    $01,s           ; drop retry code
                pshs    d             	; save buffer pointer 
                addd    #128            ; point to second half of buffer
                std     V.Buff-V.Wrtf,u ; save it
                lda     #$DB            ; retry code
                pshs    a               ; save code
                bsr     Xfr2            ; read the second sector

                puls    x               ; restore buffer pointer
                stx     V.Buff-V.Wrtf,u 
                bra     Xfr70           ; and exit

Xfr1            ldd     #256		; 256 bytes to transfer           
                std     V.Bytc-V.Wrtf,u ; save it
Xfr2            ldd     V.Lsn-V.Wrtf,u  ; LSN0?
                bne     Xfr20           ; nope, skip

Xfr10           lbsr    Brstor          ; restore to track 0

Xfr20           bsr     Select          ; select drive

                bcs     Xfr70           ; error : exit

* Calculate track number, by subtracting sectors / track from LSN, and incrementing track number
* until LSN is less than sectors / track
*

                clr     V.Track-V.Wrtf,u 	; clear track number
                clr     V.Side-V.Wrtf,u 	; clear side flag
                ldd     V.Lsn-V.Wrtf,u  	; set logical sector
                cmpd    V.T0stk-V.Wrtf,u 	; compare to max sec on t0
                bcs     Xfr40           	; branch if smaller

                subd    V.T0stk-V.Wrtf,u 	; decrement sector number
Xfr30           inc     V.Track-V.Wrtf,u 	; increment track number

                ifne    EXTFMT          
                subd    V.Stk-V.Wrtf,u  	; subtract sectors / track 	
                bcc     Xfr30           	; loop again if < 0 sectors left 

                addd    V.Stk-V.Wrtf,u  	; add sectors / tract to compensate for over-loop
                else                    
                endc                    


                ifne    DBLSIDE         
                lda     V.Fmt-V.Wrtf,u  	; get format byte
                bita    #VFmt.Dblsid		; double sided?           
                beq     Xfr40           	; no skip

* Convert logical track to physical track for double sided disk, this is done by shifting 
* dividing the track by 2, and if an odd track incrementing the head (by shifting bit 0 of 
* the track into the head).
*
                lsr     V.Track-V.Wrtf,u 	; Move bottom bit of track into carry and divide it by 2
                rol     V.Side-V.Wrtf,u 	; move bottom bit of track into side
                endc                    


                ifne    HLFSECT         
Xfr40           lda     V.Fmt-V.Wrtf,u 		; get format byte 
                bita    #VFmt.HlfSec		; halfsize (128 byte) sectors?            
                beq     Xfr90           	; nope, skip

                lslb                    	; adjust sector number
                decb                    	; a call to get sector 1 gets 1 & 2
* The software must skip sector 0                
		bra     Xfr95           	; set sector

                endc                    

Xfr90           equ     *               

                ifne    DDTRACK0        
                tst     V.DDTr0-V.Wrtf,u 	; double density on track 0?
                beq     Xfr95      		; nope     

                incb                    	; add 1 to sector number
                endc                    

Xfr95           stb     V.Sector-V.Wrtf,u 	; set sector number
Xfr50           lbsr    Settrk          	; seek to required track

                lda     V.Sector-V.Wrtf,u 	; get sector
* Extend to check of sector reg once written
* as recommended by Western Digital for the 2797
* sta SECREG,Y set it in fdc
                bsr     SetSect         ; set sector number in FDC

                lbsr    IssXfr          ; Do Transfer

                bcc     Xfr70           ; Skip If No Error

                cmpb    #E$NotRdy       ; not ready?
                orcc    #Carry		; set carry to flag error            
                beq     Xfr70           ; exit

                lsr     ,s              ; shift retry flags
                bcc     Xfr10           ; go restore and retry

                bne     Xfr50           ; just retry

Xfr70           leas    $01,s           ; drop retry flags from stack
                rts                     ; return
                                        
* Set the sector number in the WD2797
* wait for 32usec, then check it
* as recommended by Western Digital
SetSect         sta     SECREG,y	; set sector register           
                ldb     #12             ; short delay loop
SetSect1        decb                    
                bne     SetSect1        

                cmpa    SECREG,y        ; secreg set?   
                bne     SetSect         ; no, try again.......

                rts                     
                                        
* Write Sector Routine
*
                                        
Wsect           lda     #1   		; indicate write           
                sta     V.Wrtf-V.Wrtf,u 
                lda     #F.WRIT		; write command	            
                sta     V.Cmd-V.Wrtf,u  ; save it
                lbra    Xfr             ; go do transfer

                                        
* Select Drive Routine
*
Select          lda     V.Drive-V.Wrtf,u 	; get drive no
                cmpa    #DrvCnt         	; is it valid?
                bcs     Sele1           	; yes, continue

                comb                    	; error -
                ldb     #E$Unit         	; bad unit
                rts                     
                                        			
Sele1		coma                    	; active low, set other bits 
                ldb     #$01            	; set command in progress flag
                stb     V.Active-V.Wrtf,u 
		
                anda    #$FF-B.Motor-B.DPHalt 	; Setup drive control register bits ($6F)
                ldb     V.Drive-V.Wrtf,u 	; get current drive	
                cmpb    V.DrvInUse,u         	; same as drive in use ?	
                beq     L0456           	; yes, no need to do anything, skip on

                stb     V.DrvInUse,u		; Update drive in use    	     
                ldb     V.Select-V.Wrtf,u 	; get current value of select register	
                orb     #B.HeadLd		; unload the head            
                stb     >A.DskSel         	; select drive
		
                ldx     #$0014          	; short delay
L0437           leax    -$01,x          
                bne     L0437           	; wait.....

                sta     >A.DskSel         	; select new drive & spinup motor	
                tst     V.Wait-V.Wrtf,u 	; can wait by sleeping ?      
                bne     L0450           	; yes, go sleep

                ldb     #$03            
Sele7b          ldx     #25000          	; Delay 100ms
Sele8b          leax    -1,x            
                bne     Sele8b          	; inner delay loop

                decb                    
                bne     Sele7b          	; outer delay loop

                bra     L0456   		; go check drive        

L0450           ldx     #$000F    		; sleep      
                os9     F$Sleep         

L0456           anda    #$FB 			; mask out drives           	
                ldb     V.Select-V.Wrtf,u 	; get old select value
                sta     V.Select-V.Wrtf,u 	; save new value
                sta     >A.DskSel         	; set select
		
                lda     #MotTim         	; Motor timeout value
                sta     V.Timer-V.Wrtf,u 	; save it
                bitb    #B.Motor        	; is motor running ?
                beq     Sele5           	; no 

                ldb     V.Fmt-V.Wrtf,u  	; Get format byte
                bmi     Sele5           	; branch if 5.25"

                tst     V.Wait-V.Wrtf,u        	; can we wait by sleeping?
                bne     L047F           	; yes : do so

                ldb     #RdyTim         	; delay loop
Sele7           ldx     #25000          
Sele8           leax    -1,x            
                bne     Sele8           	; inner loop

                decb                    	
                bne     Sele7           	; outer loop

                bra     Sele5           	; skip over sleep

L047F           ldx     #RdyTim*5       	; sleep.....
                os9     F$Sleep         

Sele5           lda     STTREG,y        	; get controller status
		
                clrb    clear           	; carry
                bita    #NotReady           	; drive ready?
                beq     Sele4           	; ..yes

                clr     V.Active-V.Wrtf,u 	; no longer active
                comb                    	; error -
                ldb     #E$NotRdy       	; Not ready
Sele4           rts                     
                                        
*
* Seek routine
*
Settrk          lda     V.CrTrk-V.Wrtf,u 	; get current track
                clr     V.NewTrk-V.Wrtf,u 	; clear 'new track'

                ifne    DBLTRACK        
                tst     V.TwoStp-V.Wrtf,u 	; double stepping?
                beq     Sett1           	; ..no

                lsla                    	; double the track
                endc                    

Sett1           sta     TRKREG,y      		; send to WD2797     
                lda     V.Track-V.Wrtf,u 	; get desired track
                ldb     V.Fmt-V.Wrtf,u  	; get disk format byte
                bmi     Sett7           	; Branch if 5.25"

                bitb    #$04     		; check something, double step?       
                beq     Sett5           	; yes 

Sett7           cmpa    #PrcTrk80            	; Track > precomp track?
                bra     L04B0           	; skip

Sett5           cmpa    #PrcTrk40            	; Track > precomp track?
L04B0           bcs     Sett4           	; no precomp required, skip

                ldb     V.Select-V.Wrtf,u 	; get select register
                andb    #$FF-B.PreCmp           ; turn off precomp bit
                stb     V.Select-V.Wrtf,u 
                stb     >A.DskSel         
		
Sett4           cmpa    V.CrTrk-V.Wrtf,u	; is this the current track? 
                beq     Sett8           	; yes skip & send to WD

                sta     V.CrTrk-V.Wrtf,u 	; save in static store

                ifne    DBLTRACK        
                tst     V.TwoStp-V.Wrtf,u 	; double stepping?
                beq     Sett9           	; ..no

                lsla                    	; double the target track
Sett9           equ     *               
                endc                    

                ldb     #4              	; 
                stb     V.NewTrk-V.Wrtf,u 	; set newtrack to 4????
                sta     DATREG,y        	; send new track to WD
                lda     #F.SEEK         	; WD seek command
Sett3           ora     V.Step-V.Wrtf,u 	; or in step rate
                lbsr    Outcom          	; send to WD

                bsr     Sett8           	; Tell WD what current track is

                ldb     V.Status-V.Wrtf,u 	; Check status
                andb    #NotReady+SeekErr	; mask out all but not read & seek error            
                lbra    STCK            

                                        
Sett8           lda     V.CrTrk-V.Wrtf,u 	; get current track
                sta     TRKREG,y           	; tell WD
                rts                     
                                        
                                        
* Restore Drive Routine
*
*
*   input: (Y)= pointer to path decsriptor
*          (U)= pointer to global storage
*
*   if error: (B)= error code & carry is set
*

                ifne    STEPIN          
* note:  we are stepping in several tracks before
*        issuing the restore as suggested in the
*        fd 1973 application notes.
                endc                    

                                        
Brstor          lbsr    SELECT      		; Select drive    

                clr     V.CrTrk-V.Wrtf,u 	; current track = 0
                ldb     #5              	; repeat five times
RESTR2          lda     #F.STPI         	; step in command
                ora     V.Step-V.Wrtf,u 	; combine with step rate
                pshs    b               	; save counter
                bsr     Outcom          	; send to WD

                puls    b               	; restore counter
                decb                    	; decrement
                bne     RESTR2          	; loop again if counter > 0

                lda     #F.REST         	; restore command
                bra     Sett3           	; go do it

                                        
* Execute Transfer Requiring DMA
*
                                        
IssXfr          clrb                    	; clear carry	
                pshs    cc              	; save int masks
                ldb     V.Cmd-V.Wrtf,u  	; get WD command
                tst     V.Side-V.Wrtf,u 	; side 1?
                beq     L0510           	; no side 0

                orb     #SideFlag		; select side 2              
                stb     Drvbeg,u        	; save command
L0510           orcc    #IntMasks       	; turn off interrupts
                ldx     D.DMport        	; is DMA in use?
                beq     IssXfr2         	; no.....

                ldx     #1              	; wait for dma.....
                os9     F$Sleep         	

                bra     L0510           	; try again.....

IssXfr2         leax    DATREG,y           	; get data reg of WD
                stx     D.DMPort        	; set DMA to use it
		
                ldx     V.Buff-V.Wrtf,u 	; get transfer address
                stx     D.DMMem         	; tell DMA
                
		lda     V.Wrtf-V.Wrtf,u 	; get read / write flag
                sta     D.DMDir         	; tell DMA
                                        
**********
                                        
                ldx     #DAT.Task       	; task reg has DMA cpu interrupt line....
                lda     #SysTask-B.DPNMI 	; set DMA NMI line low....
                sta     ,x              	; and wake up DMA processor
                ora     #B.DPNMI        	; bring DMA NMI line high again....
                sta     ,x              	
                                        
**********
                                        

                ifne    EXTFMT          

                ifne    DDTRACK0        
                tst     V.DDTr0-V.Wrtf,u 	; track 0 double density?
                bne     IssXfr4         	; yes, skip 

                endc                    

                ldb     V.Fmt-V.Wrtf,u  	; get drive info
                tst     V.Track-V.Wrtf,u 	; cylender 0?
                bne     L0547           	; no, skip

                tst     V.Side-V.Wrtf,u 	; side 0?
                beq     IssXfr5         	; yes, single density required

L0547           bitb    #VFmt.MFM		; is this a double density disk?              	
                beq     IssXfr5         	; no, set single

                endc                    

IssXfr4         lda     V.Select-V.Wrtf,u 	; get saved select register
                anda    #$FF-B.DblDen		; set double density            	
                sta     >A.DskSel         	; set it
                sta     V.Select-V.Wrtf,u 	; and save it
		
IssXfr5         lda     V.Cmd-V.Wrtf,u  	; get WD command
                ora     V.NewTrk-V.Wrtf,u 	; add head load delay
                bsr     Outcom          	; send it

                bsr     ChkErr          	; check for errors

                ldx     #$0000          	; release DMA
                stx     D.DMPort        	
                bcc     IssXfr3         	; no error, exit

                lda     ,s              	; get old flags off stack
                ora     #Carry			; set carry flag              
                sta     ,s              	; and resave
IssXfr3         puls    pc,cc           	; restore and return

; Send FDC command in A, and wait for execution
; 
                                       
Outcom          pshs    cc         		; save condition codes     
                orcc    #IntMasks       	; disable ints
                ldb     #$FA            
                stb     V.Timer-V.Wrtf,u 
                stb     V.Active-V.Wrtf,u 
                sta     ,y              	; send command
                tst     V.Wait-V.Wrtf,u 
                bne     Fdccmd          

                                        
* Start Fdc And Wait, Hogging Cpu
IssFdc          sync                    	; loop waiting for interrupt
                lda     >INTPIA+1       	; get PIA control reg
                bita    #INTBIT         	; interrupt bit set
                beq     IssFdc          	; nope loop again	

                lda     STTREG,y        	; get status from FDC
                sta     V.Status-V.Wrtf,u 	; save it
                bra     FdcCmd3         	; return to caller

                                        
* Start FDC and Wait by Sleeping
Fdccmd          ldx     #$00C8          
                lda     V.Busy-V.Wrtf,u 
                sta     V.Wake-V.Wrtf,u 
FdcCmd1         os9     F$Sleep         

                orcc    #IntMasks       	; disable ints
                tst     V.Wake-V.Wrtf,u 	; time to wake up?
                beq     FdcCmd2         	; yep

                leax    ,x              	; X=0?
                bne     FdcCmd1         	; loop again

                clr     V.Wake-V.Wrtf,u	 
                lda     #$80            	; flag error
                sta     V.Status-V.Wrtf,u 	
                lda     #F.TERM        		; Forced interrupt FDC
                sta     CMDREG,y        	; send it
                bsr     STCK3           	; return to caller

FdcCmd3         lda     >INTPIA         	; clear interrupt from PIA
FdcCmd2         clr     V.Active-V.Wrtf,u 
                puls    pc,cc           	; restore & return
*
* Translate error status
*
ChkErr          ldb     V.Status-V.Wrtf,u	; get status 
                        
STCK            clra                    	; no error initially.....
                andb    #$FF-(Index+Busy)	; Mask out index & busy bits           
                beq     STCK3           	; branch if nothing else set, no error

SCK1            lslb                    	; shift error code 
                inca                    	; and increment error index counter
                bcc     SCK1            	; until we find an error bit

                deca                    	; make error index zero based
                leax    <ERTABLE,pcr    	; point at error table
                ldb     a,x             	; get error code
                cmpb    #E$Read			; read error ?           	
                bne     STCK2           	; nope, skip

                tst     V.Wrtf-V.Wrtf,u		; where we writing? 	
                beq     STCK2           	; no leave it as E$read

                ldb     #E$Write            	; Convert error to E$Write
STCK2           coma                    	; flag error
STCK3           rts                     	; return
                        
*			OS-9 code	 Bit set in status                 
ERTABLE         fcb     E$NotRdy	; Not ready
		fcb	E$WP		; write protect
		fcb	E$Read		; Head loaded / ecord type
		fcb	E$Seek 		; seek / record not found
                fcb     E$Read		; CRC 
		fcb	E$Read   	; Lost data
                                        
* Interrupt Request Service For Fdc
*
* Input: U=Static Storage
                                        
Fdcsrv          ldy     V.Port,u   		; get pointer to FDC regs     
                ldb     ,y              	; get status from FDC

		stb     >V.Status,u     	; save it for caller
                lda     >INTPIA         	; clear int at PIA
                lda     V.Wake,u       		; get process id to wake 
                beq     Fdcsrc2         	; process zero (none), skip

                clr     V.Wake,u        	; clear process waiting on
                ldb     #S$Wake         	; wake process up if waiting
                os9     F$Send          

Fdcsrc2         clrb                    	; flag no error
                rts                     
                                        
TimSrv          tst     V.Timer,u       	; get counter
                beq     TimSrv1         	; ..none

                tst     V.Active,u      	; command in progress?
                bne     TimSrv1         	; ..yes

                dec     V.Timer,u       	; keep count
                bne     TimSrv1         	; ..not yet

                lda     #$FF-B.DPHalt   	; turn everything off
                sta     >A.DskSel         
                sta     V.Select,u      
TimSrv1         rts               		; exit      
                                        					
                emod                    
                                        
Dkend           equ     *               
