#
# Makefile for building the OS-9 modules (and utilities) for DragonMMC
#

include beta.mk

AS=lwasm
ASFLAGS=--6809 -fos9 --pragma=condundefzero,nosymbolcase -I $(DEFS) 
ASOUTM=-o$(MODDIR)
ASOUTMN=-o$(NEWMODDIR)
ASOUTR=-o$(ROMDIR)

ASLIST=-l$(LISTDIR)

MODLIST=
DISKMODS=d0 d1 d2 d3 f0 f1 f2 f3 dd0 dd1 dd2 dd3 fd0 fd1 fd2 fd3
OS9MODS=acia51 clock init ioman os9p2 p p1 pia pipe piper pipeman rbf scf screen sysgo term t1 
NEWOS9MODS=dd
BOOTMODS=shell wd2797 boot bootdev os9p1

BETAOS9BOOTD=$(DISKDIR)/beta-os9-boot.os9
BETAOS9BOOTDD=$(DISKDIR)/beta-os9-mods_dd.os9
BETAOS9BOOTNEW=$(DISKDIR)/beta-os9-mods-new.os9

ATTRS=-e -q

.PHONY: mods newmods cmds

all: buildenv mods newmods cmds

mods: 
	$(MAKE) -f Makefile.mods 
	
newmods: 
	$(MAKE) -f Makefile.newmods 
	
cmds: 
	$(MAKE) -f Makefile.cmds

#
# ROMS.
#

ROMNAME=beta_bt
ROMNAMENEW=$(ROMNAME)_new

rom: mods
	cat $(MODDIR)/shell $(MODDIR)/wd2797 $(MODDIR)/boot $(MODDIR)/bootdev $(MODDIR)/os9p1 > $(ROMDIR)/$(ROMNAME).part
	./makerom $(ROMNAME)
	
newrom: newmods
	cat $(NEWMODDIR)/shell $(NEWMODDIR)/wd2797 $(NEWMODDIR)/boot $(NEWMODDIR)/bootdev $(NEWMODDIR)/bootdev2 $(NEWMODDIR)/os9p1 > $(ROMDIR)/$(ROMNAMENEW).part
	./makerom $(ROMNAMENEW)	

ramtest: $(MISC)/ramtest.asm
	$(AS) $(ASFLAGS) $(ASLIST)/$@.lst $(ASOUTR)/$@ $<
	rm -f $(ROMDIR)/ramtest128k.rom
	touch $(ROMDIR)/ramtest128k.rom
	os9 padrom -c255 114688 $(ROMDIR)/ramtest128k.rom
	cat $(ROMDIR)/ramtest >> $(ROMDIR)/ramtest128k.rom	
	
betatest: $(MISC)/betatest.asm
	$(AS) $(ASFLAGS) $(ASLIST)/$@.lst $(ASOUTR)/$@ $<
	rm -f $(ROMDIR)/betatest128k.rom
	touch $(ROMDIR)/betatest128k.rom
	os9 padrom -c255 114688 $(ROMDIR)/betatest128k.rom
	cat $(ROMDIR)/betatest >> $(ROMDIR)/betatest128k.rom	
	
#
# OS9boot.
#

BOOTLIST = os9p2 init ioman clock sysgo rbf scf pipeman acia51 pia screen piper \
	       d0 d1 d2 d3 f0 f1 f2 f3 dd0 dd1 dd2 dd3 fd0 fd1 fd2 fd3 term t1 p p1 pipe

NEWBOOTLIST = os9p2 init ioman clock sysgo rbf scf pipeman acia51 pia screen piper \
	       dd d0 d1 d2 d3 f0 f1 f2 f3 dd0 dd1 dd2 dd3 fd0 fd1 fd2 fd3 term t1 p p1 pipe

BOOTFILES=$(foreach file,$(BOOTLIST), $(MODDIR)/$(file))
NEWBOOTFILES=$(foreach file,$(NEWBOOTLIST), $(NEWMODDIR)/$(file))

os9boot: mods
	cat $(BOOTFILES) > $(BUILD)/os9boot

os9bootnew: newmods
	cat $(NEWBOOTFILES) > $(BUILD)/os9bootnew

	
#
# OS9 disk, unfortunately this cannot be a native Beta format disk, as os9 tools
# have no way of creating one. However since the Beta can read Dragon (64/Alpha) OS9
# format disks this should not be a problem.
# 
# os9 tools now modified to support specifying track 0 sectors.
#


os9disk: os9boot cmds	
	os9 format -e -t80 -st16 -sz10 $(BETAOS9BOOTD)
	os9 makdir $(BETAOS9BOOTD),os9mods
	os9 copy $(MODDIR)/* $(BETAOS9BOOTD),os9mods
	os9 makdir $(BETAOS9BOOTD),CMDS
	os9 copy $(CMDDIR)/* $(BETAOS9BOOTD),CMDS
	os9 copy $(CMDBINDIR)/* $(BETAOS9BOOTD),CMDS
	$(OS9ATTR_EXEC) $(foreach file,$(ALLCMDS),$(BETAOS9BOOTD),CMDS/$(file))
	os9 makdir $(BETAOS9BOOTD),SYS
	os9 copy $(SYSBINDIR)/* $(BETAOS9BOOTD),SYS
	os9 copy $(SYSDIR)/* $(BETAOS9BOOTD),SYS
	$(OS9ATTR_EXEC) $(foreach file,$(SYSCMDS),$(BETAOS9BOOTD),SYS/$(file))
	os9 copy $(BIN)/greeting $(BETAOS9BOOTD),
	os9 copy $(BIN)/startup $(BETAOS9BOOTD),
	os9 gen -b=$(BUILD)/os9boot $(BETAOS9BOOTD)

os9diskdd: os9boot cmds	
	os9 format -dr -t80 -e  $(BETAOS9BOOTDD)
	os9 makdir $(BETAOS9BOOTDD),os9mods
	os9 copy $(MODDIR)/* $(BETAOS9BOOTDD),os9mods
	os9 makdir $(BETAOS9BOOTDD),CMDS
	os9 copy $(CMDDIR)/* $(BETAOS9BOOTDD),CMDS
	os9 copy $(CMDBINDIR)/* $(BETAOS9BOOTDD),CMDS
	$(OS9ATTR_EXEC) $(foreach file,$(ALLCMDS),$(BETAOS9BOOTDD),CMDS/$(file))
	os9 makdir $(BETAOS9BOOTDD),SYS
	os9 copy $(SYSBINDIR)/* $(BETAOS9BOOTDD),SYS
	os9 copy $(SYSDIR)/* $(BETAOS9BOOTDD),SYS
	$(OS9ATTR_EXEC) $(foreach file,$(SYSCMDS),$(BETAOS9BOOTDD),SYS/$(file))
	os9 copy $(BIN)/greeting $(BETAOS9BOOTDD),
	os9 copy $(BIN)/startup $(BETAOS9BOOTDD),
	os9 gen -b=$(BUILD)/os9boot $(BETAOS9BOOTDD)

os9disknew: os9bootnew cmds
	os9 format -dr -t80 -e $(BETAOS9BOOTNEW)
	os9 makdir $(BETAOS9BOOTNEW),os9mods
	os9 copy $(MODDIR)/* $(BETAOS9BOOTNEW),os9mods
	os9 makdir $(BETAOS9BOOTNEW),CMDS
	os9 copy $(CMDDIR)/* $(BETAOS9BOOTNEW),CMDS
	os9 copy $(CMDBINDIR)/* $(BETAOS9BOOTNEW),CMDS
	$(OS9ATTR_EXEC) $(foreach file,$(ALLCMDS),$(BETAOS9BOOTNEW),CMDS/$(file))
	os9 makdir $(BETAOS9BOOTNEW),SYS
	os9 copy $(SYSBINDIR)/* $(BETAOS9BOOTNEW),SYS
	os9 copy $(SYSDIR)/* $(BETAOS9BOOTNEW),SYS
	$(OS9ATTR_EXEC) $(foreach file,$(SYSCMDS),$(BETAOS9BOOTNEW),SYS/$(file))
	os9 copy $(BIN)/greeting $(BETAOS9BOOTNEW),
	os9 copy $(BIN)/startup $(BETAOS9BOOTNEW),
	os9 gen -b=$(BUILD)/os9bootnew $(BETAOS9BOOTNEW)

buildenv:
	-mkdir $(BUILD)
	-mkdir $(MODDIR)
	-mkdir $(NEWMODDIR)
	-mkdir $(CMDDIR)
	-mkdir $(SYSDIR)
	-mkdir $(ROMDIR)
	-mkdir $(DISKDIR)
	-mkdir $(LISTDIR)
	
clean:
	rm -f $(MODDIR)/*
	rm -f $(NEWMODDIR)/*
	rm -f $(CMDDIR)/*
	rm -f $(SYSDIR)/*
	rm -f $(ROMDIR)/*
	rm -f $(DISKDIR)/*
	rm -f $(LISTDIR)/*
#	rm -f *.os9

distclean:
	rm -rf $(BUILD) 
	rm -rf $(LISTDIR)
