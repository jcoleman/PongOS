#!/usr/bin/env bash
nasm -o bootloader.bin -f bin bootloader.asm 
nasm -o kernel.bin -f bin kernel.asm 
dd if=bootloader.bin of=floppy.img bs=512
dd if=kernel.bin of=floppy.img bs=512 oseek=1
