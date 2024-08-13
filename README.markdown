What is PongOS?
===============

PongOS is a graphical version of Pong written entirely in assembly. It is designed to be booted from a floppy (image.) It was a class project for CpS 230 Computer Systems and was co-authored with Brad Yinger (GitHub at: [https://github.com/alphabj](https://github.com/alphabj).)


How to try it out:
==================

Download the `floppy.img` file from the released downloads, create a new virtual machine with a floppy drive, and mount the image file as that floppy drive. Boot the VM and have fun! 

Left paddle movement: Q/A
Right paddle movement: O/L

Using QEMU:
==========

One macOS:

```
brew install qemu
qemu-system-i386 -fda floppy.img
```

QEMU fullscreen notes:
- Uncapture mouse cursor: ctrl+opt+g
- Exit fullscreen: cmd+f

How to build:
=============
Building is accomplished by running the `buildPong.sh` file. NASM is required, as is the unix utility `dd`. The two asm source files are compiled separately and then combined together to make the floppy image file.
