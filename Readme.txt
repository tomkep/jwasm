
    1. About JWasm

    JWasm is intended to be a free MASM v6 compatible assembler. It's a
    fork of Open Watcom's WASM and released under the Sybase Open Watcom 
    Public License (see license.txt for details).

    JWasm is distributed in three packages:
    - JWASMnnB.zip : binaries for Windows (JWASM.EXE) and DOS (JWASMD.EXE,
      JWASMR.EXE).
    - JWASMnnBl.zip: binary for Linux.
    - JWASMnnS.zip : JWasm's source code. Included are makefiles to create
      binaries for Windows, DOS, OS/2, Linux and FreeBSD.

    JWasm supports various output formats:

    format     option     comment
    -----------------------------------------------------------------------
    BIN        -bin       plain binary format (boot sector, BIOS, DOS COM)
    COFF       -coff      MS-Win32 standard object format
    DOS-MZ     -mz        DOS native binary format (.EXE)
    ELF        -elf       Linux standard object format
    OMF        -omf       (default) object format, 16- and 32-bit supported


    2. Requirements

    - JWASM.EXE runs on any Win32 platform.

    - JWASMD.EXE runs in DOS 32bit protected-mode. It requires a 80386 cpu
      and needs a MS-DOS v5 compatible DOS to run (FreeDOS v1 will do).
      Long filenames (LFN) are supported.

    - JWASMR.EXE is a DOS real-mode program which runs on any x86 cpu.
      Similar to JWASMD.EXE it needs a MS-DOS v5 compatible DOS. However,
      this version doesn't support long filenames and is restricted to the
      OMF, BIN and MZ output formats.

    Memory requirements depend on the source which is assembled. The source
    itself is not kept in memory, but the symbol table is, and this table
    can easily grow to several MBs if huge amounts of equates are defined.
    That's why JWASMR.EXE might be unable to assemble large sources.


    3. JWasm Command Line Options

    Run JWasm with parameter -? or -h to make it display the options it
    will understand. Most of them are compatible with Masm, for those 
    which are specific to JWasm some additional explanations:

    -bin: if output format BIN is selected, the extension of the output
     module's default filename will be changed from .OBJ to .BIN. The
     contents of the file are just the raw data bytes emitted by the
     assembler, no header, relocations or symbol tables are generated.

    -FPi: "inline FP instructions with emulation". This will make JWasm
     to create fixups for floating-point instructions if code is 16bit.
     Some environments - Win16 and 16bit OS/2 - can then replace the FP
     instructions by calls to an FP emulator if no coprocessor exists.

    -mz: output format MZ will write a binary in DOS MZ format. The
     module's default filename will be changed from .OBJ to .EXE. All
     symbols in the module must resolve internally, no externals are allowed.
     Some values in the "MZ" header can be adjusted by directive OPTION MZ
     (see below).

    -zf<0|1>: these options select the FASTCALL calling convention type.
    The default value is MS VC style, which uses registers ECX and EDX
    for the first 2 parameters with a size <= 4.

    -Zg: this option makes JWasm to try an exact copy of Masm's code
     generation, which results in the following changes:
      - the default prologue for procedures will use
        "add [e]sp, - localsize" instead of "sub [e]sp, localsize".
      - the default epilogue for procedures will always prefer to use 
        opcode "leave".
      - expressions '<reg> == 0' and 'reg != 0' will generate code
        'or <reg>,<reg>' instead of 'cmp <reg>,0'.
      - if invoke must extend an argument's size from word to dword,
        register eax remains unchanged.

    -zlc, -zld, -zlf, -zls: these options reduce size of the output module.
     They might be useful if lots of - small - modules are to be assembled
     and put into a static library.

    -Zm: this option (or setting OPTION M510) will do:
      - set OPTION OLDSTRUCTS
      - set OPTION DOTNAME
      - set OPTION SETIF2:TRUE
      - set OPTION OFFSET:SEGMENT (if no model is set)
      - set OPTION NOSCOPED (if no model with language is set)
      - allow to define data items behind code labels
      - allow "invalid" use of REP/REPE/REPNE instruction prefixes
     Other MASM v5.1 compatibility options aren't implemented yet.

    -Zne: this option will disable syntax extensions which aren't supported
     by MASM. Currently these are:
      - IDs enclosed in backquotes
      - FASTCALL calling convention
      - floating-point immediate operands in instructions
      - directive INCBIN 
      - directives OPTION FIELDALIGN and OPTION PROCALIGN
      - directive OPTION MZ

    -zs<n>: "Set name decoration for STDCALL symbols". Options "-zs0" will 
     make object modules compatible to ALINK + Win32.lib.

    -zze: this option suppresses name decoration for procedures with
     the EXPORT attribute (exported name only).

    -zzs: this option is kind of a workaround for a WLink incompatibility.
     It's useful to be set if 1) the source module has a starting address,
     2) output format is COFF AND 3) WLink is to be used as linker.


    4. JWasm Features

    - instruction sets SSE1, SSE2, SSE3 and SSSE3 are supported.
    - supports features added in MASM v8:
      - type OWORD
      - operators IMAGEREL and SECTIONREL
      - segment attribute ALIGN(n)
      - segment characteristics INFO, READ, WRITE, EXECUTE, SHARED, NOPAGE,
        NOCACHE, DISCARD.
    - FASTCALL register calling convention supported. Fastcall styles for
      MS C and OW C supported.
    - IDs can be enclosed in back quotes (`) and thus they can contain
      characters not allowed in "normal" IDs.
    - floating-point immediate values accepted as instruction operands
      ("mov eax,1.0").
    - INCBIN directive. Syntax is 
          INCBIN filename [, starting offset[, max size]]
      filename should be enclosed in <> or double quotes.
    - the default value for structure alignment, which can be set
      by cmdline switch -Zp, can also be changed within the source
      by the FIELDALIGN parameter for the OPTION directive:
           OPTION FIELDALIGN:<1|2|4|8|16|32>
      With the new PROCALIGN parameter it's possible to automatically 
      align procedures. Syntax is:
           OPTION PROCALIGN:<1|2|4|8|16|32>
    - a numeric constant, __JWASM__, is predefined and its value is
      the current JWasm version * 100, that is, for v1.7 the value is
      170. The predefined text equate @Version won't contain JWasm's
      version, for compatibility reasons it has value <615>.
    - when starting, JWasm will search for environment variable JWASM
      and handle it similar to the way MASM handles variable ML.
    - the .MODEL directive accepts both options OS_DOS and OS_OS2.
    - output format DOS-MZ automatically creates a "MZ"-header. Directive
      OPTION MZ allows to fine-tune the values written to this header.
      The syntax for the directive is:
           OPTION MZ:[start_fixups][,header_align][,heap_min][,heap_max]
      The parameters are:
      - start_fixups: offset where segment fixups will start. The size of
        the header will always be at least this value, even if there are no
        fixups at all. Default - and minimum - value is 1Eh.
      - header_align: alignment of the header (including segment fixups). 
        Value must be a power of 2, 10h is the default and minimum.
      - heap_min: the additional space (in paragraphs) which is needed by 
        the binary to run. Default is the total of the sizes of the 
        uninitialized BSS and STACK segments.
      - heap_max: space (in paragraphs) which the binary would like to have.
        Default is FFFFh.


    5. Samples

    The binary packages contain samples in subdirectory SAMPLES.

    For output formats other than BIN or DOS-MZ, JWasm's output has to be
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
    COFF    Link      from Microsoft, will also accept OMF modules
    COFF    PoLink    supplied with PellesC, free
    ELF     LD        the GNU linker

    Note that the name of the MS OMF linker binary is LINK.EXE, identical
    to the MS COFF linker name.


    6. Debugging Support

    JWASM supports option -Zd, which will make it emit line number
    information into the object module. This allows a debugger to trace
    the binary on the source code level. Open Watcom's WD has been verified
    to work with JWASM's output. MASM's -Zi option, which additionally
    emits "type" information, isn't supported yet.


    7. MASM bugs fixed in JWasm

    - the infamous "invoke" bug: using invoke with variables of type BYTE
      (or WORD in 32bit code) causes bad code to be generated in MASM.
    - PROTOs contained twice in the source caused an EXTDEF entry to be
      generated in the object module.
    - MASM ignores an optional alignment parameter for STRUCTs which have
      just one field (and also for UNIONs, no matter how many fields it has).
      JWasm never ignores this parameter. So a STRUCT/UNION with alignment
      DWORD (or 4) and just one field of size 1 has size 1 in MASM and size
      4 in JWasm.
    - "TYPE xmm0" will return 10 in MASM v6 and v7, JWasm returns 16, same
      as MASM v8.
    - a nested structure might cause a GPF in MASM if the embedded STRUCT's
      starting offset has to be adjusted due to alignment.
    - defining huge arrays in MASM is very slow and might even cause a
      deadlock if COFF has been selected as output format.
    - for MASM v6 and v7, if an array > 64 kB is defined and output
      format OMF is selected, the array's size will be mod 0x10000 only.
    - MASM doesn't flag invalid numbers in struct/array initializer strings.
    - if an ALIAS is defined somewhere in the source and the symbol table
      is listed, a 'General Failure' error occurs in MASM if output format
      is OMF.
    - Type "coerces" for DWORD data items defined in a 32bit segment are
      ignored by MASM, i.e., "dd far16 ptr <symbol>" will generate a
      near32 fixup instead of a far16 one.
    - if the ALIGN directive has to add 5 bytes in 32bit code segments,
      MASM includes an "add eax,0" opcode, which isn't a no-op because
      flags are modified.
    - silent truncation of immediate constants: MASM v6 and v7 will accept
      line "mov [word_variable],12345h" without error.
    - preprocessed output with option -EP may erroneously contain text
      macros and macro function calls if the macros are located in the
      initialization string of a structured variable.
    - MASM generates wrong code if a conditional jump is coupled with a
      type coercion which modifies offset magnitude. Examples: "jz near32
      ptr ..." in 16bit code or "jz near16 ptr ..." in 32bit code).
    - if the arguments given to MASM end with an option which expects
      a parameter (i.e. "ml -c -Fo"), a 'General Failure' may occur.
    - floating-point data items in MASM can be followed by any suffix
      (example: REAL4 1.0foo, 2.0bar). JWasm won't accept this.

    It's slightly dangerous to fix old MASM bugs, since some code might
    work only if the bugs exists. So no, JWasm won't achieve 100% MASM
    compatibility.


    8. Known Bugs and missing Features

    a) Bugs which are known but not fixed yet:

    - JWasm won't accept a '>' in a macro parameter enclosed in <>,
      example: <"this param -> isn't valid in JWasm">

    b) Features which aren't implemented yet:

    - directives PAGE, TITLE, SUBTITLE, SUBTTL.
      the directives are ignored and a warning (level 3) is displayed.
    - the following parameters of the OPTION directive:
      - NOSIGNEXTEND
      - OLDMACROS
      - EXPR16
      - READONLY
    - optional parameter NONUNIQUE for structures is ignored.
    - types SQWORD, MMWORD, XMMWORD (MASM v8+)
    - operators LOW32, HIGH32 (MASM v8+)
    - INVOKE for procedures which use the Watcom C calling convention.
    - commandline option -Zd option for COFF/ELF output format.
    - commandline option -Zi (it's a no-op).

    c) Missing features which most likely won't be implemented:

    - %OUT directive
    - syntax "mm(n)" and "xmm(n)" (supported by MASM v6 and v7 only)


    9. How to Create the JWasm Binaries

     To create the JWasm binaries JWASMnnS is needed. The assembler is
    written in C. The following Makefiles are supplied:

    name        tool chain used               creates binary for
    ---------------------------------------------------------------
    Makefile    OW v1.7a/v1.8                 Win32, DOS (32-bit)
    OWDOS16.MAK OW v1.7a/v1.8                 DOS (16-bit)
    OWOS2.MAK   OW v1.7a/v1.8                 OS/2 (32-bit)
    OWLinux.MAK OW v1.7a/v1.8                 Linux
    MSVC.MAK    VC++ TK 2003/VC++ 2005 EE     Win32 [, DOS (32-bit)]
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
    32bit-DOS (JWASMD.EXE) binaries of JWasm.


    10. Contributors

    These people contributed to JWasm ( additions, bugfixes, bug reports):

    agner, BlackVortex, drizz, Paul Edwards, filofel, Peter Flass, gfalen,
    Japheth, Jimg, jj2007, Khusraw, Alex Kozlov, Peter Kuznetsov, misca,
    Michal Necasek, RotateRight, Ito Toshimitsu, Vortex.

    Japheth
