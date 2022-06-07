                nam     BOOTDEV         
                ttl     Device          ; Descriptor for Boot Device
                                        
                use     defsfile        
                                        
FORMAT          set     0               ; use 48 tpi Microware format
Drive           equ     0               ; drive number

                ifne    FORMAT          
                else                    
DrvSiz          set     5               ; 48 tpi
                endc                    

Sides           set     2               ; double sided
StpRat          equ     1               ; step rate 12ms
IntrLeav        equ     4               
                                        
*************************
*
*  Drive Descriptor Module
*
*
Type            set     DEVIC+OBJCT     
Revs            set     REENT+1         
                                        
                mod     DescEnd,DescName,Type,Revs,DscMgr,DscDrv 
                fcb     $FF             ; mode byte
                fcb     IOBlock/DAT.BlCt ; port bank
                fdb     DPort           ; port address
Density         set     1               ; double density

                ifne    FORMAT          
                else                    
DDTr0           set     0               ; single density on track 0
                endc                    


                ifeq    DrvSiz-5        
DrvTyp          equ     0               ; 5' drive
Cyls            set     40              
SecTrk          set     18              
SecTr0          set     18              
                endc                    

                fcb     DescName-*-1    ; initilization table size
                fcb     DT.RBF          ; device type:0=scf,1=rbf,2=pipe,3=scf
                fcb     $00             ; drive number
                fcb     $00             ; step rate
                fcb     TYP.CCF         ; drive device type
                fcb     $01             ; media density:0=single,1=double
                fdb     Cyls         	; number of cylinders (tracks)
                fcb     2               ; number of sides
                fcb     0               ; verify disk writes:0=on
                fdb     SecTrk          
                fdb     SecTr0          
                fcb     IntrLeav        ; Sector interleave offset
                fcb     8               ; Sector allocation size
DescName        equ     *               
                fcs     /bootdev2/       
DscMgr          equ     *               
                fcs     /RBF/           
DscDrv          equ     *               
                fcs     /wd2797/        
                emod                    
DescEnd         equ     *               
