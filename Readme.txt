
    1. About JWasm

    JWasm is intended to be a MASM v6 compatible, open source assembler.
    It's a fork of Open Watcom's WASM (v1.7).

    Two binaries are included, JWASM.EXE is a Win32 console application,
    JWASMD.EXE is a 32bit DOS extended application which runs in Windows,
    DOS and DosEmu.

    JWasm supports two output formats, OMF and COFF. Free OMF linkers
    are Open Watcom's WLink, Ladsoft's VALX, A. Williams' ALink and/or
    Digital Mars' OptLink. Virtually free COFF linkers are MS link (which 
    will also accept OMF, since it converts OMF modules to COFF on the fly)
    and PoLink, included in PellesC.


    2. Requirements

	Both JWASM and JWASMD need at least a 80386 cpu to run. JWASMD needs a
    DPMI host if it runs in DOS, if there's none found in memory then
    HDPMI32 will be searched.
    Memory requirements depend on the source which is assembled. The source
    itself is not kept in memory, but the symbol table is, and this table
    can easily grow to several MBs if huge amounts of equates are defined.


    3. JWasm Command Line Options

    Run JWasm with parameter -? or -h to make it display the options it 
    will understand. Some additional explanations:

    -FPi: "inline FP instructions with emulation". This will make JWasm
     to create fixups for floating-point instructions if code is 16bit.
     Some environments - Win16 and 16bit OS/2 - can then replace the FP
     instructions by calls to an FP emulator if no coprocessor exists.

    -Zm: this option (or setting OPTION M510) will do:
      - set OPTION OLDSTRUCTS
      - set OPTION NOSCOPED
      - allow to define data items behind code labels
      - allow "invalid" use of REP/REPE/REPNE instruction prefixes
     Other Masm v5.1 compatibility options aren't implemented yet.

    -zlc, -zld, -zlf, -zls: these options reduce size of the output module. 
     They might be useful if lots of - small - modules are to be assembled
     and put into a static library.
    
    -zzo: "no name decoration for STDCALL symbols". This will avoid to add
     a '_' prefix and '@size' suffix to STDCALL symbols. Makes object modules
     written by JWasm compatible to ALINK + Win32.lib.

    -zzs: this option is kind of a workaround for a WLink incompatibility. 
     It's useful to be set if 1) the source module has a starting address,
     2) output format is COFF AND 3) WLink is to be used as linker.


    4. MASM bugs fixed in JWasm

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
    - "TYPE xmm0" will return 10 in Masm v6 and v7, JWasm returns 16, same
      as Masm v8.
    - a nested structure might cause a GPF in Masm if the embedded STRUCT's
      starting offset has to be adjusted due to alignment.
    - defining huge arrays in Masm is very slow and might even cause a 
      deadlock if COFF has been selected as output format. 
    - Masm doesn't flag invalid numbers in struct/array initializer strings.
    - if an ALIAS is defined somewhere in the source and the symbol table
      is listed, a 'General Failure' error occurs in Masm if output format
      is OMF.

    It's slightly dangerous to fix old MASM bugs, since some code might
    work only if the bugs exists. So no, JWasm won't achieve 100% MASM
    compatibility.


    5. JWasm Features

    - support for 16 byte OWORD data type.
    - .XMM2 and .XMM3 supported.
    - WATCOM register calling convention.
    - a numeric constant, __JWASM__, is predefined and its value is
      the current JWasm version * 100, that is, for v1.7 the value is
      170. The predefined text equate @Version won't contain JWasm's
      version, for compatibility reasons it has value <615>.


    6. How to Create the JWasm Binaries

     Binary and source are split in 2 packages, JWASMxxB and JWASMxxS. To
    create the JWasm binary JWASMxxS is needed. The assembler is written
    in C, as default Open Watcom is used for compilation, but a makefile
    for MS VC (MSVC.MAK, tested with MS VC Toolkit 2003) is also supplied.
     The default Makefile is intended to be run with Open Watcom's WMAKE
    (please read the comments in that file before trying to run WMAKE).
    The default settings will create both JWASM.EXE and JWASMD.EXE. This 
    can be changed by modifying variables WIN= and/or DOS= in the Makefile. 


    7. Samples

    JWASMxxB contains Win32 samples in subdirectory SAMPLES. Some of these
    samples will use include files contained in MASM32. JWasm should be
    compatible with WINDOWS.INC of MASM32, so there's no problem. 

    JWasm is also compatible with Win32Inc, a Public Domain version of
    Win32 include files in MASM syntax. Just in case you're worried about
    some odd claims in the MASM32 license.


    8. Debugging Support

    JWASM supports option -Zd, which will make it emit line number 
    information into the object module. This allows a debugger to trace
    the binary on the source code level. Open Watcom's WD has been verified
    to work with JWASM's output. MASM's -Zi option, which additionally
    emits "type" information, isn't supported yet.


    9. Known Bugs and missing Features

    These bugs are known but not fixed yet:

    - JWasm won't accept a '>' in a macro parameter enclosed in <>,
      example: <"this param -> isn't valid in JWasm">
    - JWasm doesn't know "type expressions". So "TYPE <type>" will just 
      give the size of <type> as a constant.
    - INVOKE doesn't support the Watcom C calling convention.
    - -Zd option works with OMF output format only.

    These Masm features aren't implemented yet:

    - OPTIONs:
      - CASEMAP:NOTPUBLIC
      - OLDMACROS/NOOLDMACROS
      - SETIF2
      - EXPR16
      - NOSIGNEXTEND
      - OFFSET:SEGMENT
      - READONLY/NOREADONLY
    - .RADIX
    - .ALPHA
    - PUSHCONTEXT, POPCONTEXT
    - types SQWORD, MMWORD, XMMWORD (Masm v8+)
    - operators LOW32, HIGH32 (Masm v8+)

    Listing support in JWasm is limited yet, the directives in the following
    list are ignored and a warning (level 4) might be seen:
    .TFCOND, .SFCOND, .LFCOND, .LISTIF, .NOLISTIF, .LISTMACRO, .NOLISTMACRO,
    .LISTMACROALL, .LISTALL, .XALL, .LALL, .SALL, PAGE, TITLE, SUBTITLE, 
    SUBTTL.


    10. Roadmap

    - implement support for ELF object format
    - implement full debugging support (option -Zi)
    - implement missing directives


    11. License

    Read LICENSE.TXT for details.

