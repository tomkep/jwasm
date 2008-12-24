
    1. About JWasm

    JWasm is intended to be a MASM v6 compatible, open source assembler.
    It's a fork of Open Watcom's WASM (v1.7).

    JWasm is distributed in three packages:
    - JWASMnnB.zip : binaries for Windows (JWASM.EXE) and DOS (JWASMD.EXE)
    - JWASMnnBl.zip: binary for Linux
    - JWASMnnS.zip : JWasm's source code

    JWasm natively supports output formats BIN, OMF, COFF and ELF.


    2. Requirements

    JWasm needs at least a 80386 cpu to run.

    JWASM.EXE runs on Win32 platforms. JWASMD.EXE needs a MS-DOS v5
    compatible DOS to run (FreeDOS v1 will do as well). Long filenames are
    supported.

    Memory requirements depend on the source which is assembled. The source
    itself is not kept in memory, but the symbol table is, and this table
    can easily grow to several MBs if huge amounts of equates are defined.


    3. JWasm Command Line Options

    Run JWasm with parameter -? or -h to make it display the options it
    will understand. Some additional explanations:

    -bin: if output format BIN is selected, the extension of the output
     module's default filename will be changed from .OBJ to .BIN. The
     contents of the file are just the raw data bytes emitted by the
     assembler, no header, relocations or symbol tables are generated.

    -FPi: "inline FP instructions with emulation". This will make JWasm
     to create fixups for floating-point instructions if code is 16bit.
     Some environments - Win16 and 16bit OS/2 - can then replace the FP
     instructions by calls to an FP emulator if no coprocessor exists.

    -zlc, -zld, -zlf, -zls: these options reduce size of the output module.
     They might be useful if lots of - small - modules are to be assembled
     and put into a static library.

    -Zm: this option (or setting OPTION M510) will do:
      - set OPTION OLDSTRUCTS
      - set OPTION NOSCOPED
      - set OPTION SETIF2:TRUE
      - allow to define data items behind code labels
      - allow "invalid" use of REP/REPE/REPNE instruction prefixes
     Other MASM v5.1 compatibility options aren't implemented yet.

    -Zne: this option will disable syntax extensions which aren't supported
     by MASM. Currently these are:
      - IDs enclosed in backquotes
      - WATCOM_C calling convention
      - floating-point immediate operands in instructions
      - directive INCBIN 
      - directives OPTION FIELDALIGN and OPTION PROCALIGN
    
    -zzo: "no name decoration for STDCALL symbols". This will avoid to add
     a '_' prefix and '@size' suffix to STDCALL symbols. Makes object modules
     written by JWasm compatible to ALINK + Win32.lib.

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
    - WATCOM_C register calling convention supported.
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


    5. Samples

    The binary packages contain samples in subdirectory SAMPLES.

    As far as programming for Win32 is concerned: JWasm should be
    compatible with recent versions of both Win32Inc and Masm32.

    For output formats other than BIN, JWasm's output has to be linked to
    create an executable binary. Select one of the following linkers:

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
    - PURGE actually works in JWasm.
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
      - OFFSET:SEGMENT
      - NOSIGNEXTEND
      - OLDMACROS
      - EXPR16
      - READONLY
    - types SQWORD, MMWORD, XMMWORD (MASM v8+)
    - operators LOW32, HIGH32 (MASM v8+)
    - INVOKE doesn't support the Watcom C calling convention.
    - commandline option -Zd option works with OMF output format only.
    - commandline option -Zi is a no-op.

    c) Missing features which most likely won't be implemented:

    - %OUT directive
    - syntax "mm(n)" and "xmm(n)" (MASM v6 and v7 only)


    9. How to Create the JWasm Binaries

     To create the JWasm binaries JWASMnnS is needed. The assembler is
    written in C. The following Makefiles are supplied:

    name        tool chain used                 creates binary for
    --------------------------------------------------------------
    Makefile    Open Watcom v1.7a               Win32, DOS
    MSVC.MAK    MS VC Toolkit 2003 or MS VC 6   Win32, DOS
    MinGW.MAK   MinGW with GCC v3.4.5           Win32
    OWLnx.MAK   Open Watcom v1.7a               Linux

     The default settings in Makefile will create both the Windows and DOS
    binaries of JWasm (this can be changed by modifying variables WIN= and/or
    DOS= ). Makefile, MinGW.MAK and OWLnx.MAK are supposed to be run with
    Open Watcom's WMake! Please read the comments in these file before trying
    to run WMake!


    10. License

    Read LICENSE.TXT for details.

