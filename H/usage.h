"   JWasm [options] asm-file [options] [asm-file] ... [@env_var]\n"
"\n"
"options:\n"
#if AMD64_SUPPORT
"-<0|1|..|7>[p]      Set CPU: 0=8086 (default), 1=80186, 2=80286, 3=80386,\n"
"                    4=80486, 5=Pentium, 6=Pentium Pro, 7=x86-64.\n"
"                    <p> allows privileged instructions.\n"
#else
"-<0|1|..|6>[p]      Set CPU: 0=8086 (default), 1=80186, 2=80286, 3=80386,\n"
"                    4=80486, 5=Pentium, 6=Pentium Pro.\n"
"                    <p> allows privileged instructions.\n"
#endif
#if BIN_SUPPORT
"-bin                Generate plain binary file\n"
#endif
"-c                  Assemble without linking (always set)\n"
#if COFF_SUPPORT
"-coff               Generate COFF format object file\n"
#endif
"-C<p|u|x>           set OPTION CASEMAP: p=NONE, u=ALL,\n"
"                    x=NOTPUBLIC (default).\n"
"-D<name>[=text]     Define text macro\n"
#ifdef DEBUG_OUT
"-d6                 Display debug trace on stdout\n"
#endif
"-e<number>          Set error limit number (default=50)\n"
#if ELF_SUPPORT
"-elf                Generate ELF format object file\n"
#endif
"-EP                 Output preprocessed listing to stdout\n"
"-Fi<file_name>      Force <file_name> to be included\n"
"-Fl[=<file_name>]   Write listing file\n"
"-Fo<file_name>      Set object file name\n"
"-Fr<file_name>      Set error file name\n"
"-FPi                80x87 instructions with emulation fixups\n"
"-FPi87              80x87 instructions (default)\n"
"-fpc                Disallow floating-point instructions (.NO87)\n"
"-fp<n>              Set FPU, <n> is: 0=8087 (default), 2=80287, 3=80387\n"
"-G<c|d|z>           Use Pascal, C or Stdcall calling convention\n"
"-I<directory>       Add directory to list of include directories\n"
"-j                  Force signed types to be used for signed values\n"
#ifdef DEBUG_OUT
"-ls                 Display preprocessed line storage on stdout\n"
#endif
"-m<t|s|m|c|l|h|f>   Set memory model:\n"
"                    (Tiny, Small, Medium, Compact, Large, Huge, Flat)\n"
#if BIN_SUPPORT
#if MZ_SUPPORT
"-mz                 Generate binary in DOS MZ format\n"
#endif
#endif
"-n<d|m|t>=<name>    Set name of data segment, module or text segment\n"
#if COCTALS
"-o                  Allow C form of octal constants\n"
#endif
"-omf                Generate OMF format object file (default)\n"
#ifdef DEBUG_OUT
"-pm=<n>             Stop assembly after <n> passes\n"
#endif
"-q, -nologo         Don't display version and copyright information\n"
"-Sa                 Maximize source listing\n"
"-Sg                 Display generated code in listing\n"
"-Sn                 Suppress symbol-table listing\n"
"-Sx                 List false conditionals\n"
"-w                  Same as /W0 /WX\n"
"-W<number>          Set warning level number (default=2, max=4)\n"
#if AMD64_SUPPORT
"-win64              Generate 64bit COFF format object file\n"
#endif
"-WX                 Treat all warnings as errors\n"
"-X                  Ignore INCLUDE environment path\n"
"-zcm                C names are decorated with '_' prefix (default)\n"
"-zcw                No name decoration for C symbols\n"
"-Zd                 Add line number debug information\n"
"-Zf                 Make all symbols public\n"
"-zf<0|1>            Set FASTCALL type: 0=MS VC style (default),\n"
"                    1=OW register calling convention\n"
"-Zg                 Generated code is to exactly match Masm's one\n"
"-Zi                 Symbolic debug info (not implemented yet)\n"
"-zlc                No OMF records about data in code\n"
"-zld                No OMF records about file dependencies\n"
"-zlf                No COFF .file entry in symbol table\n"
"-zls                No COFF auxiliary entries for sections in symbol table\n"
"-Zm                 Masm v5.1 compatibility\n"
"-Zne                Disable syntax extensions not supported by Masm\n"
"-Zp[n]              Set structure alignment, n=<1|2|4|8|16|32>\n"
"-zs<0|1|2>          Set STDCALL symbol decoration: 0=No name decoration,\n"
"                    1=No '@size' suffix for functions, 2=Full (default)\n"
"-zze                No name decoration for exported symbols\n"
"-zzs                Store decorated name of start address (COFF only)\n"
"@env_var            Environment variable or file containing further commands\n"
