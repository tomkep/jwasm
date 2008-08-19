/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  tokens recognized by directives
*
****************************************************************************/

// Strings recognized by directives

fix( TOK_READONLY,      "READONLY",     0,              0               ),
fix( TOK_BYTE,          "BYTE",         ALIGN_BYTE,     INIT_ALIGN      ),
fix( TOK_WORD,          "WORD",         ALIGN_WORD,     INIT_ALIGN      ),
fix( TOK_DWORD,         "DWORD",        ALIGN_DWORD,    INIT_ALIGN      ),
fix( TOK_PARA,          "PARA",         ALIGN_PARA,     INIT_ALIGN      ),
fix( TOK_PAGE,          "PAGE",         ALIGN_PAGE,     INIT_ALIGN      ),
fix( TOK_PRIVATE,       "PRIVATE",      COMB_INVALID,   INIT_COMBINE    ),
fix( TOK_PUBLIC,        "PUBLIC",       COMB_ADDOFF,    INIT_COMBINE    ),
fix( TOK_STACK,         "STACK",        COMB_STACK,     INIT_COMBINE    ),
fix( TOK_COMMON,        "COMMON",       COMB_COMMON,    INIT_COMBINE    ),
fix( TOK_MEMORY,        "MEMORY",       COMB_ADDOFF,    INIT_COMBINE    ),
fix( TOK_AT,            "AT",           COMB_INVALID,   INIT_COMBINE    ),
fix( TOK_USE16,         "USE16",        FALSE,          INIT_USE        ),
fix( TOK_USE32,         "USE32",        TRUE,           INIT_USE        ),
fix( TOK_CLASS,         NULL,           0,              INIT_CLASS      ),
fix( TOK_TINY,          "TINY",         MOD_TINY,       INIT_MODEL      ),
fix( TOK_SMALL,         "SMALL",        MOD_SMALL,      INIT_MODEL      ),
fix( TOK_COMPACT,       "COMPACT",      MOD_COMPACT,    INIT_MODEL      ),
fix( TOK_MEDIUM,        "MEDIUM",       MOD_MEDIUM,     INIT_MODEL      ),
fix( TOK_LARGE,         "LARGE",        MOD_LARGE,      INIT_MODEL      ),
fix( TOK_HUGE,          "HUGE",         MOD_HUGE,       INIT_MODEL      ),
fix( TOK_FLAT,          "FLAT",         MOD_FLAT,       INIT_MODEL      ),
fix( TOK_NEARSTACK,     "NEARSTACK",    STACK_NEAR,     INIT_STACK      ),
fix( TOK_FARSTACK,      "FARSTACK",     STACK_FAR,      INIT_STACK      ),
fix( TOK_PROC_BASIC,    "BASIC",        LANG_BASIC,     0               ),
fix( TOK_PROC_FORTRAN,  "FORTRAN",      LANG_FORTRAN,   0               ),
fix( TOK_PROC_PASCAL,   "PASCAL",       LANG_PASCAL,    0               ),
fix( TOK_PROC_C,        "C",            LANG_C,         0               ),
fix( TOK_PROC_WATCOM_C, "WATCOM_C",     LANG_WATCOM_C,  0               ),
fix( TOK_PROC_STDCALL,  "STDCALL",      LANG_STDCALL,   0               ),
fix( TOK_PROC_SYSCALL,  "SYSCALL",      LANG_SYSCALL,   0               ),
fix( TOK_OS_OS2,        "OS_OS2",       OPSYS_OS2,      0               ),
fix( TOK_OS_DOS,        "OS_DOS",       OPSYS_DOS,      0               ),
