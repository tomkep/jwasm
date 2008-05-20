
    1. About JWasm

    JWasm is intended to be a MASM v6 compatible, open source assembler.
    It's a fork of Open Watcom's WASM (v1.7).

    Two binaries are included, JWASM.EXE is a Win32 console application,
    JWASMD.EXE is a 32bit DOS extended application which runs in Windows,
    DOS and DosEmu.


    2. Requirements

	Both JWASM and JWASMD need at least a 80386 cpu to run. JWASMD needs a
    DPMI host if it runs in DOS, if there's none found in memory then
    HDPMI32 will be searched.
    Memory requirements depend on the source which is assembled. The source
    itself is not kept in memory, but the symbol table is, and this table
    can easily grow to several MBs if huge amounts of equates are defined.


    3. JWasm Command Line Options

    Running JWasm without parameter will display the options JWasm will
    understand. Some additional explanations:

    -bt=<os>: this is an Open Watcom peculiarity which will make JWasm
     search for an environment variable named <os>_INCLUDE and - if one
     is found - add the content to the include search path. If -bt is not
     set, JWasm will assume that target OS is the same as host OS.

    -FPi: "inline FP instructions with emulation". This will make JWasm
     to generate normal FP instructions, but additionally, if the code is
     16bit, it creates fixups which allow some OSes to replace the FP
     instructions by calls to an FP emulator.


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
    create JWASM.EXE and/or JWASMD.EXE JWASMxxS is needed. It contains a
    Makefile intended to be run with WMAKE (please read the comments in that
    file before trying to run WMAKE). The default settings will create
    both JWASM.EXE and JWASMD.EXE. This can be changed by modifying 
    variables WIN= and/or DOS= in the Makefile. Open Watcom is needed to
    make the binaries.


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
    - labels in lines containing a "conditional assembly" directive
      (IF, ELSE, ENDIF, ...) will be ignored.
    - a superfluous type name is not always skipped, resulting in an
      error msg of "Symbol <TYPE>.<TYPE> is not defined". Workaround:
      remove the type name.
    - the following syntax is not accepted:
        mov eax, [edi.<TYPE>].<Field_name>
      a possible workaround is to change it to:
        mov eax, [edi].<TYPE>.<Field_name>
    - a macro function must always be called with parameters enclosed in
      brackets, even if EXITM will return nothing.

    These Masm features aren't implemented yet:

    - GOTO (jump to a macro label) 
    - THIS operator
    - .UNTILCXZ directive
    - OPTIONs:
      - CASEMAP:NOTPUBLIC
      - M510/NOM510
      - SCOPE/NOSCOPE
      - OLDMACROS/NOOLDMACROS
      - OLDSTRUCTS/NOOLDSTRUCTS
      - SETIF2
      - LANGUAGE
      - LJMP/NOLJMP
      - EXPR16
      - EMULATOR/NOEMULATOR
      - NOSIGNEXTEND
      - OFFSET:SEGMENT
      - READONLY/NOREADONLY
      - SEGMENT:USE16|USE32|FLAT
    - .RADIX, .SEQ, .ALPHA, .TFCOND, .SFCOND, .LFCOND
    - PUSHCONTEXT, POPCONTEXT

    There is no full listing support in JWasm, so all directives
    related to listings are ignored and a warning is displayed.


    10. Roadmap

    - speed optimization
    - implement full listing
    - implement full debugging support (option -Zi)
    - implement support for COFF object format
    - implement operator THIS
    - implement HLL directive .UNTILCXZ


    11. License

    Read LICENSE.TXT for details.

