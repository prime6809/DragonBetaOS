******************************
*
* Drive Descriptor Module
*
*
		use		defsfile
		
Type	set		DEVIC+OBJCT
Revs	set		REENT+1
		mod		DescEnd,DescName,Type,Revs,DscMgr,DscDrv
		fcb		DIR.+SHARE.+PREAD.+PWRIT.+UPDAT.+EXEC.+PEXEC.

		fcb		IOBlock/DAT.BlCt	; Port bank
		fdb		DPort				; port address
;Density	set		1					; Double density
;Sides	set		2					; Double sided
;DrvTyp	set		0					; 5.25" drive
;Cyls	set		40
;SecTrk	set		16
;SecTr0	set		10
;SAS		set		8
		
		fcb		DescName-*-1		; bytes of options
		fcb		DT.RBF
		fcb		Drive				; RBF device & drive no
		fcb		StpRat
		fcb		DrvTyp
		fcb		Density
		fdb		Cyls				; number of Cylinders
		fcb		Sides
		fcb		0					; verify turned on
		fdb		SecTrk
		fdb		SecTr0
		fcb		IntrLeav			; Sector interleave offset
		fcb		SAS					; Sector allocation size

DescName set	*
		ifdef NAMED
		fcb		'd'
		fcb		'0'+Drive+$80
		fcb		0					; name padding
		endc
		
		ifdef NAMEF
		fcb		'f'
		fcb		'0'+Drive+$80
		fcb		0					; name padding
		endc
		
		ifdef NAMEDD
		fcc		"dd"
		fcb		'0'+Drive+$80
		endc

		ifdef NAMEFD
		fcc		"fd"
		fcb		'0'+Drive+$80
		endc
		
		ifdef DEFAULT
		fcs		"dd"
		fcb		0
		endc

		ifdef BOOTDEV
		fcs     "bootdev"
		endc

		ifdef BOOTDEV2
		fcs     "bootdev2"
		endc
		
		
DscMgr	set		*
		fcs		"RBF"				; rbfman
		
DscDrv	set		*
		fcs		"wd2797"
				
		emod
DescEnd	equ		*		
		
		