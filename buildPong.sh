#!/usr/bin/env bash
nasm -o bootloader.bin -f bin bootloader.asm 
nasm -o kernel.bin -f bin kernel.asm 
# make a completely blank disk image
dd if=/dev/zero of=floppy.img bs=512 count=2880
dd if=bootloader.bin of=floppy.img bs=512 count=1 conv=notrunc
dd if=kernel.bin of=floppy.img bs=512 seek=1 conv=notrunc
