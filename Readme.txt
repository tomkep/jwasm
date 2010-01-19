

    1. About JWasm

    JWasm is intended to be a free MASM compatible assembler. It's a fork
    of Open Watcom's WASM and released under the Sybase Open Watcom Public
    License (see license.txt for details).

    JWasm supports various output formats:

    format     option     comment
    -----------------------------------------------------------------------
    BIN        -bin       plain binary format (boot sector, BIOS, DOS COM)
    COFF       -coff      MS Win32 object format
    WIN64      -win64     MS Win64 object format
    MZ         -mz        DOS native binary format (.EXE)
    ELF32      -elf       Linux 32-bit object format
    ELF64      -elf64     Linux 64-bit object format
    OMF        -omf       (default) object format, 16- and 32-bit supported

    JWasm is distributed in several packages. Besides the source package
    there are some which contain precompiled binaries for Windows, DOS and
    Linux. Other OSes like OS/2 and FreeBSD are also supported, but no binary
    is supplied, it must be created from the sources.


    2. Requirements

    - JWASM.EXE, a precompiled Win32 binary, runs on any Win32 platform.

    - JWASMD.EXE, a precompiled DOS binary, runs in DOS 32bit protected-mode.
      It requires a 80386 cpu and needs a MS-DOS v5 compatible DOS to run 
      (FreeDOS v1 will do). Long filenames (LFN) are supported.

    - JWASMR.EXE is a DOS real-mode program which runs on any x86 cpu.
      Similar to JWASMD.EXE it needs a MS-DOS v5 compatible DOS. Restrictions
      of this version are:
      - doesn't support long filenames
      - supports OMF, BIN and MZ output formats only
      - no support for 64-bit

    Memory requirements depend on the source which is assembled. The source
    itself is not kept in memory, but the symbol table is, and this table
    can easily grow to several MBs if huge amounts of equates are defined.
    That's why JWASMR.EXE might be unable to assemble large sources.


    3. Documentation

    JWasm's documentation consists of

    - Readme.txt (this file)
    - Manual.txt, which describes the differences to Masm.
    - History.txt, which describes bugfixes and changes of all JWasm versions.
    - License.txt, which is a copy of the Sybase Open Watcom Public License.


    4. Samples

    The binary packages contain samples in subdirectory SAMPLES.

    For output formats other than BIN or MZ, JWasm's output has to be
    linked to create an executable binary. Select one of the following
    linkers:

    Format  Linker    Comment
    -------------------------------------------------------------
    OMF     WLink     contained in Open Watcom, free, open source
    OMF     ValX      by David Lindauer (Ladsoft), free
    OMF     OptLink   from Digital Mars, free
    OMF     ALink     by A. Williams, free
    OMF     Link16    the old OMF linker (v5.60) from Microsoft
    OMF     TLink     from Borland
    COFF    WLink     this linker accepts OMF and COFF modules
    COFF    MS Link   will also accept OMF modules
    COFF    PoLink    supplied with PellesC, free
    ELF32   LD        the GNU linker
    WIN64   MS Link   must be version 7.10 or newer
    WIN64   Polink    must be version 5 or newer
    ELF64   LD        the GNU linker

    Note that the name of the MS OMF linker binary is LINK.EXE, identical
    to the MS COFF linker name.


    5. How to Create the JWasm Binaries

     JWasm is written in C. The following Makefiles are supplied
    in the source package: 

    name        tool chain used               creates binary for
    ---------------------------------------------------------------
    Makefile    OW v1.7a/v1.8                 Win32, DOS (32-bit)
    OWDOS16.MAK OW v1.7a/v1.8                 DOS (16-bit)
    OWOS2.MAK   OW v1.7a/v1.8                 OS/2 (32-bit)
    OWLinux.MAK OW v1.7a/v1.8                 Linux
    MSVC.MAK    VC++ TK 2003/VC++ 2005 EE     Win32 [, DOS (32-bit)]
    PCC.MAK     PCC                           Win32
    PellesC.MAK Pelles C v5                   Win32 [, DOS (32-bit)]
    BCC.MAK     Borland C++ Cmdline Tools     Win32
    GccWin.MAK  GCC, MinGW/Cygwin             Win32
    GccDos.MAK  GCC, DGPJJ                    DOS (32-bit)
    GccUnix.MAK GCC, FreeBSD/Linux            FreeBSD [, Linux]
    TCC.MAK     Tiny C                        Win32

     The makefiles which use the OW tool chain are supposed to be run
    with Open Watcom's WMake, MSVC.MAK should be run with MS NMAKE, 
    PellesC.MAK expects POMAKE and for Gcc*.MAK GNU make is to be used.

    More detailed information may be found in the makefiles themselves!

     The default settings in Makefile will create both the Win32 and
    32bit-DOS binaries of JWasm.


    6. Using JWasm with Visual Studio 2005/2008/2010/...

    To integrate JWasm into VS, copy file jwasm.rules to directory
    <vc_dir>\VC\VCProjectDefaults. After this is done, JWasm can be
    selected as assembler inside the VS IDE.


    7. Contributors

    These people contributed to JWasm ( additions, bugfixes, bug reports):

    agner, BlackVortex, drizz, Paul Edwards, filofel, Peter Flass,
    James C. Fuller, gfalen, Japheth, Jimg, jj2007, Khusraw, Alex Kozlov,
    Peter Kuznetsov, misca, Michal Necasek, H. Nidudsson, RotateRight, 
    Ito Toshimitsu, Vortex.

    Japheth

