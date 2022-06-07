# DragonBetaOS
Butildable OS and roms for the Dragon Beta

This archive contains a set of buildable files to enable you to build the
operating system ROMs and disks for the Dragon 128 / project Beta.

The following are buildable from this archive :

The Beta boot rom, that is plugged into the motherboard, this has part of
OS-9 built into it plus code to initialize the hardware and allow the rest
of the operating system to be booted from disk. 

The operating system "kernel" which is stored on disk in OS9Boot, and loaded 
at boot time, this contains the rest of the operating system plus the bulk 
of the hardware drivers.

The operating system commands, usually contained in the CMDS directory on
the boot disk. Most of these are built from disassembled commented source 
code. However as some of the commands where originally written in a high
level language (mostly C), these are supplied in binary form.

Memory test and setup rom, this will probe for and test any memory on the 
motherboard. Optionally it can log results to the serial port.

Graphics test ROM and mini-monitor.

In it's basic form this archive will build a binary identical set of roms
and commands to those extracted from the original machine & disks. However
you can optionally build an 'enhanced' version of the boot rom and kernel
that allow niceties such as booting from a standard Dragon format OS-9 disk
and having a NitrOS style /dd default for the boot device.


Tools required to build :

A unix like shell environment, native on Linux, or Mac, using builtin terminal
programs. For Windows the two main options are Cygwin, or Windows Services for 
Linux, either should be able to be made to work.

OS9 toolshed : https://sourceforge.net/projects/toolshed/
LWasm : http://www.lwtools.ca/

These will both need to be installed on the target platform.

