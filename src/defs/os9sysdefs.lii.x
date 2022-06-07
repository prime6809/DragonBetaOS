                opt     -l              
                ttl     Direct          ; Page Definitions
                                        
************************************************************
*
*     Edition History
*
* Date       Change
* ========   ===============================================
* 83/04/21   Addition of D.Daywk for day of week for 7
*            function Com-Trol clock.
* 83/05/04   Added D.ModEnd and D.ModDAT pointers        WGP
* 83/05/04   Changed MD$MBNum to MD$MPDAT for NCM        WGP
* 83/06/01   Suspend state defn added for P$State        WGP
* 83/06/30   Added D.CldRes vector for MotGED restart after
*            RAM, ROM search                             WGP
*
                page                    
************************************************************
*
*     Direct Page Variable Definitions
*
                org     $20             
D.Tasks         rmb     2               ; Task User Table
D.TmpDAT        rmb     2               ; Temporary DAT Image stack
D.Init          rmb     2               ; Initialization Module ptr
D.Poll          rmb     2               ; Interrupt Polling Routine ptr
D.Time          equ     .               ; System Time
D.Year          rmb     1               
D.Month         rmb     1               
D.Day           rmb     1               
D.Hour          rmb     1               
D.Min           rmb     1               
D.Sec           rmb     1               
D.Tick          rmb     1               
D.Slice         rmb     1               ; current slice remaining
D.TSlice        rmb     1               ; Ticks per Slice
D.Boot          rmb     1               ; Bootstrap attempted flag
D.MotOn         rmb     1               ; Floppy Disk Motor-On time out
D.ErrCod        rmb     1               ; Reset Error Code
D.Daywk         rmb     1               ; day of week, com-trol clock
**********
*
* Extensions for the Dragon 128
*
D.DMPort        rmb     2               ; port address for DMA
D.DMMem         rmb     2               ; memory address for DMA
D.DMDir         rmb     1               ; direction of DMA (1=write to port)
D.TimTbl        rmb     2               ; address of timer polling table
D.GRReg         rmb     1               ; current value of graphics control port
D.LtPen         rmb     1               ; current value of control reg with light pen IRQ
D.Baud          rmb     1               ; current value of baud rate port
*
**********
                org     $40             
D.BlkMap        rmb     4               ; Memory Block Map ptr
D.ModDir        rmb     4               ; Module Directory ptrs
D.PrcDBT        rmb     2               ; Process Descriptor Block Table ptr
D.SysPrc        rmb     2               ; System Process Descriptor ptr
D.SysDAT        rmb     2               ; System DAT Image ptr
D.SysMem        rmb     2               ; System Memory Map ptr
D.Proc          rmb     2               ; Current Process ptr
D.AProcQ        rmb     2               ; Active Process Queue
D.WProcQ        rmb     2               ; Waiting Process Queue
D.SProcQ        rmb     2               ; Sleeping Process Queue
D.ModEnd        rmb     2               ; Module Directory end ptr
D.ModDAT        rmb     2               ; Module Dir DAT image end ptr
D.CldRes        rmb     2               ; Cold Restart vector
                org     $80             
D.DevTbl        rmb     2               ; I/O Device Table
D.PolTbl        rmb     2               ; I/O Polling Table
                rmb     4               ; reserved
D.PthDBT        rmb     2               ; Path Descriptor Block Table ptr
D.DMAReq        rmb     1               ; DMA Request flag
                org     $C0             
D.SysSvc        rmb     2               ; System Service Routine entry
D.SysDis        rmb     2               ; System Service Dispatch Table ptr
D.SysIRQ        rmb     2               ; System IRQ Routine entry
D.UsrSvc        rmb     2               ; User Service Routine entry
D.UsrDis        rmb     2               ; User Service Dispatch Table ptr
D.UsrIRQ        rmb     2               ; User IRQ Routine entry
D.SysStk        rmb     2               ; System stack
D.SvcIRQ        rmb     2               ; In-System IRQ service
D.SysTsk        rmb     1               ; System Task number
                org     $E0             
D.Clock         rmb     2               
D.XSWI3         rmb     2               
D.XSWI2         rmb     2               
D.XFIRQ         rmb     2               
D.XIRQ          rmb     2               
D.XSWI          rmb     2               
D.XNMI          rmb     2               
D.ErrRst        rmb     2               
                org     $F2             
D.SWI3          rmb     2               
D.SWI2          rmb     2               
D.FIRQ          rmb     2               
D.IRQ           rmb     2               
D.SWI           rmb     2               
D.NMI           rmb     2               
*
*   Block Map flags
*
NotRAM          equ     %10000000       ; Block Not RAM flag
ModBlock        equ     %00000010       ; Module in Block
RAMinUse        equ     %00000001       ; RAM Block in use flag
*
*   Service Dispatch Table special entries
*
IOEntry         equ     254             
                page                    
************************************************************
*
*     Process Descriptor Definitions
*
DefIOSiz        equ     16              ; Default I/O Data Length
NumPaths        equ     16              ; Number of Local Paths

                ifne    EXTERR          
ErrNamSz        equ     32              ; size of error messages path name area
                endc                    

                                        
                org     0               
P$ID            rmb     1               ; Process ID
P$PID           rmb     1               ; Parent's ID
P$SID           rmb     1               ; Sibling's ID
P$CID           rmb     1               ; Child's ID
P$SP            rmb     2               ; Stack ptr
P$Task          rmb     1               ; Task Number
P$PagCnt        rmb     1               ; Memory Page Count
P$User          rmb     2               ; User Index
P$Prior         rmb     1               ; Priority
P$Age           rmb     1               ; Age
P$State         rmb     1               ; Status
P$Queue         rmb     2               ; Queue Link (Process ptr)
P$IOQP          rmb     1               ; Previous I/O Queue Link (Process ID)
P$IOQN          rmb     1               ; Next I/O Queue Link (Process ID)
P$PModul        rmb     2               ; Primary Module
P$SWI           rmb     2               ; SWI Entry Point
P$SWI2          rmb     2               ; SWI2 Entry Point
P$SWI3          rmb     2               ; SWI3 Entry Point
P$Signal        rmb     1               ; Signal Code
P$SigVec        rmb     2               ; Signal Intercept Vector
P$SigDat        rmb     2               ; Signal Intercept Data Address
P$DeadLk        rmb     1               ; Dominant proc ID if I/O locked
                rmb     $20-.           ; unused
P$DIO           rmb     DefIOSiz        ; Default I/O ptrs
P$Path          rmb     NumPaths        ; I/O Path Table
P$DATImg        rmb     64              ; DAT Image
P$Links         rmb     32              ; Block Link counts

                ifne    EXTERR          
P$ErrNam        rmb     ErrNamSz        ; area for error messages path name
                endc                    

                rmb     $200-.          ; Local stack
P$Stack         equ     .               ; Top of Stack
P$Size          equ     .               ; Size of Process Descriptor
                                        
*
*   Process State Flags
*
SysState        equ     %10000000       
TimSleep        equ     %01000000       
TimOut          equ     %00100000       
ImgChg          equ     %00010000       
Suspend         equ     %00001000       
Condem          equ     %00000010       
Dead            equ     %00000001       
                page                    
************************************************************
*
*     Module Directory Entry Definitions
*
                org     0               
MD$MPDAT        rmb     2               ; Module DAT Image ptr
MD$MBSiz        rmb     2               ; Memory Block size
MD$MPtr         rmb     2               ; Module ptr
MD$Link         rmb     2               ; Module Link count
MD$ESize        equ     .               ; Module Directory Entry size
                                        
MD$MBNum        equ     MD$MPDAT        ; this equate for old file assembly
                                        
                opt     l               
