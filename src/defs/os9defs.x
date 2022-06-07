                opt     -l              
                ttl     OS-9            ; System Symbol Definitions
                                        
***************
* Edition History
                                        
*   Date    Changes Made
* -------- ----------------------------------------------
* 83/03/01 Six new Status codes added: SS.BlkRd - SS.ELog
* 83/03/08 New status code added: SS.SSig
* 83/04/21 added new F$ call for 7 day clock function
*          for com-trol level II.
* 83/05/04 F$GCMDir service request added for NCM  by  WGP
* 83/06/29 Added ProtFlag to init module offsets
                                        
                pag                     
************************************************************
*
*     System Service Request Code Definitions
*
                org     0               
F$Link          rmb     1               ; Link to Module
F$Load          rmb     1               ; Load Module from File
F$UnLink        rmb     1               ; Unlink Module
F$Fork          rmb     1               ; Start New Process
F$Wait          rmb     1               ; Wait for Child Process to Die
F$Chain         rmb     1               ; Chain Process to New Module
F$Exit          rmb     1               ; Terminate Process
F$Mem           rmb     1               ; Set Memory Size
F$Send          rmb     1               ; Send Signal to Process
F$Icpt          rmb     1               ; Set Signal Intercept
F$Sleep         rmb     1               ; Suspend Process
F$SSpd          rmb     1               ; Suspend Process
F$ID            rmb     1               ; Return Process ID
F$SPrior        rmb     1               ; Set Process Priority
F$SSWI          rmb     1               ; Set Software Interrupt
F$PErr          rmb     1               ; Print Error
F$PrsNam        rmb     1               ; Parse Pathlist Name
F$CmpNam        rmb     1               ; Compare Two Names
F$SchBit        rmb     1               ; Search Bit Map
F$AllBit        rmb     1               ; Allocate in Bit Map
F$DelBit        rmb     1               ; Deallocate in Bit Map
F$Time          rmb     1               ; Get Current Time
F$STime         rmb     1               ; Set Current Time
F$CRC           rmb     1               ; Generate CRC
F$GPrDsc        rmb     1               ; get Process Descriptor copy
F$GBlkMp        rmb     1               ; get System Block Map copy
F$GModDr        rmb     1               ; get Module Directory copy
F$CpyMem        rmb     1               ; Copy External Memory
F$SUser         rmb     1               ; Set User ID number
F$UnLoad        rmb     1               ; Unlink Module by name
F$Ctime         rmb     1               ; Com-Trol clock read
F$Cstime        rmb     1               ; Com-Trol set time
F$CTswi2        rmb     1               ; Com-Trol double postbyte swi2

                ifne    EXTERR          
F$InsErr        rmb     1               ; install error messages path/module
                endc                    

                org     $28             ; Beginning of System Reserved Calls
F$SRqMem        rmb     1               ; System Memory Request
F$SRtMem        rmb     1               ; System Memory Return
F$IRQ           rmb     1               ; Enter IRQ Polling Table
F$IOQu          rmb     1               ; Enter I/O Queue
F$AProc         rmb     1               ; Enter Active Process Queue
F$NProc         rmb     1               ; Start Next Process
F$VModul        rmb     1               ; Validate Module
F$Find64        rmb     1               ; Find Process/Path Descriptor
F$All64         rmb     1               ; Allocate Process/Path Descriptor
F$Ret64         rmb     1               ; Return Process/Path Descriptor
F$SSvc          rmb     1               ; Service Request Table Initialization
F$IODel         rmb     1               ; Delete I/O Module
F$SLink         rmb     1               ; System Link
F$Boot          rmb     1               ; Bootstrap System
F$BtMem         rmb     1               ; Bootstrap Memory Request
F$GProcP        rmb     1               ; Get Process ptr
F$Move          rmb     1               ; Move Data (low bound first)
F$AllRAM        rmb     1               ; Allocate RAM blocks
F$AllImg        rmb     1               ; Allocate Image RAM blocks
F$DelImg        rmb     1               ; Deallocate Image RAM blocks
F$SetImg        rmb     1               ; Set Process DAT Image
F$FreeLB        rmb     1               ; Get Free Low Block
F$FreeHB        rmb     1               ; Get Free High Block
F$AllTsk        rmb     1               ; Allocate Process Task number
F$DelTsk        rmb     1               ; Deallocate Process Task number
F$SetTsk        rmb     1               ; Set Process Task DAT registers
F$ResTsk        rmb     1               ; Reserve Task number
F$RelTsk        rmb     1               ; Release Task number
F$DATLog        rmb     1               ; Convert DAT Block/Offset to Logical
***** With non-contiguous modules this system call will become obsolete
F$DATTmp        rmb     1               ; Make temporary DAT image
*****
F$LDAXY         rmb     1               ; Load A [X,[Y]]
F$LDAXYP        rmb     1               ; Load A [X+,[Y]]
F$LDDDXY        rmb     1               ; Load D [D+X,[Y]]
F$LDABX         rmb     1               ; Load A from 0,X in task B
F$STABX         rmb     1               ; Store A at 0,X in task B
F$AllPrc        rmb     1               ; Allocate Process Descriptor
F$DelPrc        rmb     1               ; Deallocate Process Descriptor
F$ELink         rmb     1               ; Link using Module Directory Entry
F$FModul        rmb     1               ; Find Module Directory Entry
F$MapBlk        rmb     1               ; Map Specific Block
F$ClrBlk        rmb     1               ; Clear Specific Block
F$DelRAM        rmb     1               ; Deallocate RAM blocks
F$GCMDir        rmb     1               ; Pack module directory
**********
* Extensions for the Dragon 128
*
F$Timer         rmb     1               ; Install timer routine
F$GMap          rmb     1               ; request graphics memory
F$GClr          rmb     1               ; return graphics memory
*
**********
                pag                     
************************************************************
*
*     I/O Service Request Code Definitions
*
                org     $80             
I$Attach        rmb     1               ; Attach I/O Device
I$Detach        rmb     1               ; Detach I/O Device
I$Dup           rmb     1               ; Duplicate Path
I$Create        rmb     1               ; Create New File
I$Open          rmb     1               ; Open Existing File
I$MakDir        rmb     1               ; Make Directory File
I$ChgDir        rmb     1               ; Change Default Directory
I$Delete        rmb     1               ; Delete File
I$Seek          rmb     1               ; Change Current Position
I$Read          rmb     1               ; Read Data
I$Write         rmb     1               ; Write Data
I$ReadLn        rmb     1               ; Read Line of ASCII Data
I$WritLn        rmb     1               ; Write Line of ASCII Data
I$GetStt        rmb     1               ; Get Path Status
I$SetStt        rmb     1               ; Set Path Status
I$Close         rmb     1               ; Close Path
I$DeletX        rmb     1               ; Delete from current exec dir
                                        
                                        
************************************************************
*
*      File Access Modes
*
READ.           equ     %00000001       
WRITE.          equ     %00000010       
UPDAT.          equ     READ.+WRITE.    
EXEC.           equ     %00000100       
PREAD.          equ     %00001000       
PWRIT.          equ     %00010000       
PEXEC.          equ     %00100000       
SHARE.          equ     %01000000       
DIR.            equ     %10000000       
                                        
                pag                     
************************************************************
*
*     Signal Codes
*
                org     0               
S$Kill          rmb     1               ; Non-Interceptable Abort
S$Wake          rmb     1               ; Wake-up Sleeping Process
S$Abort         rmb     1               ; Keyboard Abort
S$Intrpt        rmb     1               ; Keyboard Interrupt
                                        
                                        
************************************************************
*
*     Status Codes for GetStat/PutStat
*
                org     0               
SS.Opt          rmb     1               ; Read/Write PD Options
SS.Ready        rmb     1               ; Check for Device Ready
SS.Size         rmb     1               ; Read/Write File Size
SS.Reset        rmb     1               ; Device Restore
SS.WTrk         rmb     1               ; Device Write Track
SS.Pos          rmb     1               ; Get File Current Position
SS.EOF          rmb     1               ; Test for End of File
SS.Link         rmb     1               ; Link to Status routines
SS.ULink        rmb     1               ; Unlink Status routines
SS.Feed         rmb     1               ; issue form feed
SS.Frz          rmb     1               ; Freeze DD. information
SS.SPT          rmb     1               ; Set DD.TKS to given value
SS.SQD          rmb     1               ; Sequence down hard disk
SS.DCmd         rmb     1               ; Send direct command to disk
SS.DevNm        rmb     1               ; Return Device name (32-bytes at [X])
SS.FD           rmb     1               ; Return File Descriptor (Y-bytes at [X])
SS.Ticks        rmb     1               ; Set Lockout honor duration
SS.Lock         rmb     1               ; Lock/Release record
SS.DStat        rmb     1               ; Return Display Status (CoCo)
SS.Joy          rmb     1               ; Return Joystick Value (CoCo)
SS.BlkRd        rmb     1               ; Block Read
SS.BlkWr        rmb     1               ; Block Write
SS.Reten        rmb     1               ; Retension cycle
SS.WFM          rmb     1               ; Write File Mark
SS.RFM          rmb     1               ; Read past File Mark
SS.ELog         rmb     1               ; Read Error Log
SS.SSig         rmb     1               ; Send signal on data ready
SS.Relea        rmb     1               ; Release device
SS.Edit         rmb     1               ; Edit line (SCF)
SS.SMac         rmb     1               ; Set macro (SCF)
SS.CMac         rmb     1               ; Clear macro (SCF)
SS.Baud         rmb     1               ; select baud rate (b.r. in X)
SS.Mouse        rmb     1               ; get/reset mouse position (Dragon 128)
                                        
                ttl     Structure       ; Formats
                pag                     
************************************************************
*
*     Module Definitions
*
*     Universal Module Offsets
*
                org     0               
M$ID            rmb     2               ; ID Code
M$Size          rmb     2               ; Module Size
M$Name          rmb     2               ; Module Name
M$Type          rmb     1               ; Type / Language
M$Revs          rmb     1               ; Attributes / Revision Level
M$Parity        rmb     1               ; Header Parity
M$IDSize        equ     .               ; Module ID Size
*
*     Type-Dependent Module Offsets
*
*   System, File Manager, Device Driver, Program Module
*
M$Exec          rmb     2               ; Execution Entry Offset
*
*   Device Driver, Program Module
*
M$Mem           rmb     2               ; Stack Requirement
*
*   Device Driver, Device Descriptor Module
*
M$Mode          rmb     1               ; Device Driver Mode Capabilities
*
*   Device Descriptor Module
*
                org     M$IDSize        
M$FMgr          rmb     2               ; File Manager Name Offset
M$PDev          rmb     2               ; Device Driver Name Offset
                rmb     1               ; M$Mode (defined above)
M$Port          rmb     3               ; Port Address
M$Opt           rmb     1               ; Device Default Options
M$DTyp          rmb     1               ; Device Type
*
*   Configuration Module Entry Offsets
*
                org     M$IDSize        
MaxMem          rmb     3               ; Maximum Free Memory
PollCnt         rmb     1               ; Entries in Interrupt Polling Table
DevCnt          rmb     1               ; Entries in Device Table
InitStr         rmb     2               ; Initial Module Name
SysStr          rmb     2               ; System Device Name
StdStr          rmb     2               ; Standard I/O Pathlist
BootStr         rmb     2               ; Bootstrap Module name

                ifne    EXTERR          
ErrStr          rmb     2               ; error messages path name
                endc                    

ProtFlag        rmb     1               ; Write protect enable flag
                pag                     
************************************************************
*
*     Module Field Definitions
*
*   ID Field
*
M$ID1           equ     $87             ; Module ID code byte one
M$ID2           equ     $CD             ; Module ID code byte two
M$ID12          equ     M$ID1*256+M$ID2 
*
*   Module Type / Language byte
*
*   Field Masks
*
TypeMask        equ     %11110000       ; Type Field
LangMask        equ     %00001111       ; Language Field
*
*   Type Values
*
Devic           equ     $F0             ; Device Descriptor Module
Drivr           equ     $E0             ; Physical Device Driver
FlMgr           equ     $D0             ; File Manager
Systm           equ     $C0             ; System Module
Data            equ     $40             ; Data Module
Multi           equ     $30             ; Multi-Module
Sbrtn           equ     $20             ; Subroutine Module
Prgrm           equ     $10             ; Program Module
*
*   Language Values
*
Objct           equ     1               ; 6809 Object Code Module
ICode           equ     2               ; Basic09 I-code
PCode           equ     3               ; Pascal P-code
CCode           equ     4               ; C I-code
CblCode         equ     5               ; Cobol I-code
FrtnCode        equ     6               ; Fortran I-code
*
*   Module Attributes / Revision byte
*
*   Field Masks
*
AttrMask        equ     %11110000       ; Attributes Field
RevsMask        equ     %00001111       ; Revision Level Field
*
*   Attribute Flags
*
ReEnt           equ     %10000000       ; Re-Entrant Module
*
*   Device Type Values
*
DT.SCF          equ     0               ; Sequential Character File Type
DT.RBF          equ     1               ; Random Block File Type
DT.Pipe         equ     2               ; Pipe File Type
*
*   CRC Result Constant
*
CRCCon1         equ     $80             
CRCCon23        equ     $0FE3           
                pag                     
************************************************************
*
*   Machine Characteristics Definitions
*
R$CC            equ     0               ; Condition Codes register
R$A             equ     1               ; A Accumulator
R$B             equ     2               ; B Accumulator
R$D             equ     R$A             ; Combined A:B Accumulator
R$DP            equ     3               ; Direct Page register
R$X             equ     4               ; X Index register
R$Y             equ     6               ; Y Index register
R$U             equ     8               ; User Stack register
R$PC            equ     10              ; Program Counter register
R$Size          equ     12              ; Total register package size
                                        
Entire          equ     %10000000       ; Full Register Stack flag
FIRQMask        equ     %01000000       ; Fast-Interrupt Mask bit
HalfCrry        equ     %00100000       ; Half Carry flag
IRQMask         equ     %00010000       ; Interrupt Mask bit
Negative        equ     %00001000       ; Negative flag
Zero            equ     %00000100       ; Zero flag
TwosOvfl        equ     %00000010       ; Two's Comp Overflow flag
Carry           equ     %00000001       ; Carry bit
IntMasks        equ     IRQMask+FIRQMask 
Sign            equ     %10000000       ; sign bit
                                        
true            equ     1               ; useful name
false           equ     0               ; useful name
                                        
                                        
                ttl     Error           ; Code Definitions
                pag                     
************************************************************
*
*     Error Code Definitions
*
                org     200             
E$PthFul        rmb     1               ; Path Table full
E$BPNum         rmb     1               ; Bad Path Number
E$Poll          rmb     1               ; Polling Table Full
E$BMode         rmb     1               ; Bad Mode
E$DevOvf        rmb     1               ; Device Table Overflow
E$BMID          rmb     1               ; Bad Module ID
E$DirFul        rmb     1               ; Module Directory Full
E$MemFul        rmb     1               ; Process Memory Full
E$UnkSvc        rmb     1               ; Unknown Service Code
E$ModBsy        rmb     1               ; Module Busy
E$BPAddr        rmb     1               ; Bad Page Address
E$EOF           rmb     1               ; End of File
                rmb     1               
E$NES           rmb     1               ; Non-Existing Segment
E$FNA           rmb     1               ; File Not Accesible
E$BPNam         rmb     1               ; Bad Path Name
E$PNNF          rmb     1               ; Path Name Not Found
E$SLF           rmb     1               ; Segment List Full
E$CEF           rmb     1               ; Creating Existing File
E$IBA           rmb     1               ; Illegal Block Address
                rmb     1               
E$MNF           rmb     1               ; Module Not Found
                rmb     1               
E$DelSP         rmb     1               ; Deleting Stack Pointer memory
E$IPrcID        rmb     1               ; Illegal Process ID
                rmb     1               
E$NoChld        rmb     1               ; No Children
E$ISWI          rmb     1               ; Illegal SWI code
E$PrcAbt        rmb     1               ; Process Aborted
E$PrcFul        rmb     1               ; Process Table Full
E$IForkP        rmb     1               ; Illegal Fork Parameter
E$KwnMod        rmb     1               ; Known Module
E$BMCRC         rmb     1               ; Bad Module CRC
E$USigP         rmb     1               ; Unprocessed Signal Pending
E$NEMod         rmb     1               ; Non Existing Module
                                        
E$BNam          rmb     1               ; Bad Name
E$BMHP          rmb     1               ; (bad module header parity)
E$NoRam         rmb     1               ; No Ram Available
E$BPrcID        rmb     1               ; Bad Process ID
E$NoTask        rmb     1               ; No available Task number
                                        
                rmb     $F0-.           ; reserved
                                        
E$Unit          rmb     1               ; Illegal Unit (drive)
E$Sect          rmb     1               ; Bad SECTor number
E$WP            rmb     1               ; Write Protect
E$CRC           rmb     1               ; Bad Check Sum
E$Read          rmb     1               ; Read Error
E$Write         rmb     1               ; Write Error
E$NotRdy        rmb     1               ; Device Not Ready
E$Seek          rmb     1               ; Seek Error
E$Full          rmb     1               ; Media Full
E$BTyp          rmb     1               ; Bad Type (incompatable) media
E$DevBsy        rmb     1               ; Device Busy
E$DIDC          rmb     1               ; Disk ID Change
E$Lock          rmb     1               ; Record is busy (locked out)
E$Share         rmb     1               ; Non-sharable file busy
E$DeadLk        rmb     1               ; I/O Deadlock error
                                        
                opt     l               
                                        
