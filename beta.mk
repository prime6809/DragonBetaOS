#
# Folder defs for Dragon Beta OS9 Makefiles.
#

BUILD=build
SRC=src
BIN=bin

LISTDIR=listing

# Source directories
CMDSRC=$(SRC)/cmds
MODSRC=$(SRC)/mods
GRFSRC=$(SRC)/graphics
SYSSRC=$(SRC)/sys
DEFS=$(SRC)/defs
MISC=$(SRC)/misc

# Binary dirs copied to disks
CMDBINDIR=$(BIN)/cmds
SYSBINDIR=$(BIN)/sys


# Build directories
CMDDIR=$(BUILD)/cmds
MODDIR=$(BUILD)/mods
SYSDIR=$(BUILD)/sys
NEWMODDIR=$(BUILD)/newmods
ROMDIR=$(BUILD)/roms

DISKDIR=$(BUILD)/disk

# commands built from source
CMDS=attr backup binex build copy date del deldir dir display dsave dump echo edit \
	 exbin free format graphics ident link list load login makdir mdir merge mfree \
	 procs pwd pxd rename save setime shell sleep tee tmode tmode tsmon unlink utils \
	 verify xmode

#comands included as binaries
BINCMDS=asm basic09 callmenu cmp d debug dircopy patch runb wait

SYSCMDS=errmsg

ALLCMDS=$(CMDS) $(BINCMDS)

# borrowed from Nitros makefiles....
OS9ATTR			= os9 attr -q
OS9ATTR_TEXT	= $(OS9ATTR) -npe -npw -pr -ne -w -r
OS9ATTR_EXEC	= $(OS9ATTR) -pe -npw -pr -e -w -r
