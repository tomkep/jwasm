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
* Description:  table of non-instruction reserved words:
*               - registers,
*               - operators (unary + binary),
*               - predefined types
*               - directives
*               for instructions see instruct.h!
*
****************************************************************************/

/* v1.96: items needn't be sorted anymore!
 * The items are stored in structures of type asm_ins.
 * That's why the field names (rm_byte, op2, opcode, prefix) are a bit strange.
 * If an item is inserted, moved or deleted, the project needs
 * a full rebuild.
 */

/* token str len   rm_byte          op2      opcode    flags  cpu     op1 */

res(AL,   al, 2, RWT_REGISTER,      OP_AL,      0,       0,   P_86,    0)
res(CL,   cl, 2, RWT_REGISTER,      OP_CL,      1,       0,   P_86,    0)
res(DL,   dl, 2, RWT_REGISTER,      OP_R8,      2,       0,   P_86,    0)
res(BL,   bl, 2, RWT_REGISTER,      OP_R8,      3,       0,   P_86,    0)
res(AH,   ah, 2, RWT_REGISTER,      OP_R8,      4,       0,   P_86,    0)
res(CH,   ch, 2, RWT_REGISTER,      OP_R8,      5,       0,   P_86,    0)
res(DH,   dh, 2, RWT_REGISTER,      OP_R8,      6,       0,   P_86,    0)
res(BH,   bh, 2, RWT_REGISTER,      OP_R8,      7,       0,   P_86,    0)

res(AX,   ax, 2, RWT_REGISTER,      OP_AX,      0,       0,   P_86,    0)
res(CX,   cx, 2, RWT_REGISTER,      OP_R16,     1,       0,   P_86,    0)
res(DX,   dx, 2, RWT_REGISTER,      OP_DX,      2,       0,   P_86,    0)
res(BX,   bx, 2, RWT_REGISTER,      OP_R16,     3,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(SP,   sp, 2, RWT_REGISTER,      OP_R16,     4,       0,   P_86,    0)
res(BP,   bp, 2, RWT_REGISTER,      OP_R16,     5,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(SI,   si, 2, RWT_REGISTER,      OP_R16,     6,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(DI,   di, 2, RWT_REGISTER,      OP_R16,     7,       0,   P_86,    SFR_IREG|SFR_SIZ2)

res(EAX, eax, 3, RWT_REGISTER,      OP_EAX,     0,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ECX, ecx, 3, RWT_REGISTER,      OP_R32,     1,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EDX, edx, 3, RWT_REGISTER,      OP_R32,     2,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EBX, ebx, 3, RWT_REGISTER,      OP_R32,     3,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ESP, esp, 3, RWT_REGISTER,      OP_R32,     4,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EBP, ebp, 3, RWT_REGISTER,      OP_R32,     5,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ESI, esi, 3, RWT_REGISTER,      OP_R32,     6,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EDI, edi, 3, RWT_REGISTER,      OP_R32,     7,       0,   P_386,   SFR_IREG|SFR_SIZ4)

res(ES,   es, 2, RWT_REGISTER,      OP_SR86,    0,       0,   P_86,    0)
res(CS,   cs, 2, RWT_REGISTER,      OP_SR86,    1,       0,   P_86,    0)
res(SS,   ss, 2, RWT_REGISTER,      OP_SR86,    2,       0,   P_86,    0)
res(DS,   ds, 2, RWT_REGISTER,      OP_SR86,    3,       0,   P_86,    0)
res(FS,   fs, 2, RWT_REGISTER,      OP_SR386,   4,       0,   P_386,   0)
res(GS,   gs, 2, RWT_REGISTER,      OP_SR386,   5,       0,   P_386,   0)

res(ST,   st, 2, RWT_REGISTER,      OP_ST,      0,       0,   P_87,    0)

res(MM0, mm0, 3, RWT_REGISTER,      OP_MMX,     0,       0,   P_MMX,   0)
res(MM1, mm1, 3, RWT_REGISTER,      OP_MMX,     1,       0,   P_MMX,   0)
res(MM2, mm2, 3, RWT_REGISTER,      OP_MMX,     2,       0,   P_MMX,   0)
res(MM3, mm3, 3, RWT_REGISTER,      OP_MMX,     3,       0,   P_MMX,   0)
res(MM4, mm4, 3, RWT_REGISTER,      OP_MMX,     4,       0,   P_MMX,   0)
res(MM5, mm5, 3, RWT_REGISTER,      OP_MMX,     5,       0,   P_MMX,   0)
res(MM6, mm6, 3, RWT_REGISTER,      OP_MMX,     6,       0,   P_MMX,   0)
res(MM7, mm7, 3, RWT_REGISTER,      OP_MMX,     7,       0,   P_MMX,   0)

res(XMM0, xmm0, 4, RWT_REGISTER,    OP_XMM,     0,       0,   P_SSE1,  0)
res(XMM1, xmm1, 4, RWT_REGISTER,    OP_XMM,     1,       0,   P_SSE1,  0)
res(XMM2, xmm2, 4, RWT_REGISTER,    OP_XMM,     2,       0,   P_SSE1,  0)
res(XMM3, xmm3, 4, RWT_REGISTER,    OP_XMM,     3,       0,   P_SSE1,  0)
res(XMM4, xmm4, 4, RWT_REGISTER,    OP_XMM,     4,       0,   P_SSE1,  0)
res(XMM5, xmm5, 4, RWT_REGISTER,    OP_XMM,     5,       0,   P_SSE1,  0)
res(XMM6, xmm6, 4, RWT_REGISTER,    OP_XMM,     6,       0,   P_SSE1,  0)
res(XMM7, xmm7, 4, RWT_REGISTER,    OP_XMM,     7,       0,   P_SSE1,  0)

res(CR0, cr0, 3, RWT_REGISTER,      OP_CR,      0,       0,   P_386,   0)
res(CR2, cr2, 3, RWT_REGISTER,      OP_CR,      2,       0,   P_386,   0)
res(CR3, cr3, 3, RWT_REGISTER,      OP_CR,      3,       0,   P_386,   0)
res(CR4, cr4, 3, RWT_REGISTER,      OP_CR,      4,       0,   P_586,   0)
res(DR0, dr0, 3, RWT_REGISTER,      OP_DR,      0,       0,   P_386,   0)
res(DR1, dr1, 3, RWT_REGISTER,      OP_DR,      1,       0,   P_386,   0)
res(DR2, dr2, 3, RWT_REGISTER,      OP_DR,      2,       0,   P_386,   0)
res(DR3, dr3, 3, RWT_REGISTER,      OP_DR,      3,       0,   P_386,   0)
res(DR6, dr6, 3, RWT_REGISTER,      OP_DR,      6,       0,   P_386,   0)
res(DR7, dr7, 3, RWT_REGISTER,      OP_DR,      7,       0,   P_386,   0)
res(TR3, tr3, 3, RWT_REGISTER,      OP_TR,      3,       0,   P_486,   0)
res(TR4, tr4, 3, RWT_REGISTER,      OP_TR,      4,       0,   P_486,   0)
res(TR5, tr5, 3, RWT_REGISTER,      OP_TR,      5,       0,   P_486,   0)
res(TR6, tr6, 3, RWT_REGISTER,      OP_TR,      6,       0,   P_386,   0)
res(TR7, tr7, 3, RWT_REGISTER,      OP_TR,      7,       0,   P_386,   0)

#if AMD64_SUPPORT

/* for simplicity, all x64 reserved words must be consecutive
 * ( see Set64Bit() in parser.c ).
 */

res(SPL, spl, 3, RWT_REGISTER,      OP_R8,      4, RWF_X64,    P_64,   0)
res(BPL, bpl, 3, RWT_REGISTER,      OP_R8,      5, RWF_X64,    P_64,   0)
res(SIL, sil, 3, RWT_REGISTER,      OP_R8,      6, RWF_X64,    P_64,   0)
res(DIL, dil, 3, RWT_REGISTER,      OP_R8,      7, RWF_X64,    P_64,   0)
res(R8B, r8b, 3, RWT_REGISTER,      OP_R8,      8, RWF_X64,    P_64,   0)
res(R9B, r9b, 3, RWT_REGISTER,      OP_R8,      9, RWF_X64,    P_64,   0)
res(R10B,r10b,4, RWT_REGISTER,      OP_R8,     10, RWF_X64,    P_64,   0)
res(R11B,r11b,4, RWT_REGISTER,      OP_R8,     11, RWF_X64,    P_64,   0)
res(R12B,r12b,4, RWT_REGISTER,      OP_R8,     12, RWF_X64,    P_64,   0)
res(R13B,r13b,4, RWT_REGISTER,      OP_R8,     13, RWF_X64,    P_64,   0)
res(R14B,r14b,4, RWT_REGISTER,      OP_R8,     14, RWF_X64,    P_64,   0)
res(R15B,r15b,4, RWT_REGISTER,      OP_R8,     15, RWF_X64,    P_64,   0)

res(R8W, r8w, 3, RWT_REGISTER,      OP_R16,     8, RWF_X64,    P_64,   0)
res(R9W, r9w, 3, RWT_REGISTER,      OP_R16,     9, RWF_X64,    P_64,   0)
res(R10W,r10w,4, RWT_REGISTER,      OP_R16,    10, RWF_X64,    P_64,   0)
res(R11W,r11w,4, RWT_REGISTER,      OP_R16,    11, RWF_X64,    P_64,   0)
res(R12W,r12w,4, RWT_REGISTER,      OP_R16,    12, RWF_X64,    P_64,   0)
res(R13W,r13w,4, RWT_REGISTER,      OP_R16,    13, RWF_X64,    P_64,   0)
res(R14W,r14w,4, RWT_REGISTER,      OP_R16,    14, RWF_X64,    P_64,   0)
res(R15W,r15w,4, RWT_REGISTER,      OP_R16,    15, RWF_X64,    P_64,   0)

res(R8D, r8d, 3, RWT_REGISTER,      OP_R32,     8, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R9D, r9d, 3, RWT_REGISTER,      OP_R32,     9, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R10D,r10d,4, RWT_REGISTER,      OP_R32,    10, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R11D,r11d,4, RWT_REGISTER,      OP_R32,    11, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R12D,r12d,4, RWT_REGISTER,      OP_R32,    12, RWF_X64,    P_64,   0)
res(R13D,r13d,4, RWT_REGISTER,      OP_R32,    13, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R14D,r14d,4, RWT_REGISTER,      OP_R32,    14, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R15D,r15d,4, RWT_REGISTER,      OP_R32,    15, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)

res(RAX, rax, 3, RWT_REGISTER,      OP_RAX,     0, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RCX, rcx, 3, RWT_REGISTER,      OP_R64,     1, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RDX, rdx, 3, RWT_REGISTER,      OP_R64,     2, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RBX, rbx, 3, RWT_REGISTER,      OP_R64,     3, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RSP, rsp, 3, RWT_REGISTER,      OP_R64,     4, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RBP, rbp, 3, RWT_REGISTER,      OP_R64,     5, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RSI, rsi, 3, RWT_REGISTER,      OP_R64,     6, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RDI, rdi, 3, RWT_REGISTER,      OP_R64,     7, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R8,  r8,  2, RWT_REGISTER,      OP_R64,     8, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R9,  r9,  2, RWT_REGISTER,      OP_R64,     9, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R10, r10, 3, RWT_REGISTER,      OP_R64,    10, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R11, r11, 3, RWT_REGISTER,      OP_R64,    11, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R12, r12, 3, RWT_REGISTER,      OP_R64,    12, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R13, r13, 3, RWT_REGISTER,      OP_R64,    13, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R14, r14, 3, RWT_REGISTER,      OP_R64,    14, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R15, r15, 3, RWT_REGISTER,      OP_R64,    15, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)

res(XMM8, xmm8, 4, RWT_REGISTER,    OP_XMM,     8, RWF_X64,    P_64,   0)
res(XMM9, xmm9, 4, RWT_REGISTER,    OP_XMM,     9, RWF_X64,    P_64,   0)
res(XMM10,xmm10,5, RWT_REGISTER,    OP_XMM,    10, RWF_X64,    P_64,   0)
res(XMM11,xmm11,5, RWT_REGISTER,    OP_XMM,    11, RWF_X64,    P_64,   0)
res(XMM12,xmm12,5, RWT_REGISTER,    OP_XMM,    12, RWF_X64,    P_64,   0)
res(XMM13,xmm13,5, RWT_REGISTER,    OP_XMM,    13, RWF_X64,    P_64,   0)
res(XMM14,xmm14,5, RWT_REGISTER,    OP_XMM,    14, RWF_X64,    P_64,   0)
res(XMM15,xmm15,5, RWT_REGISTER,    OP_XMM,    15, RWF_X64,    P_64,   0)

res(CR8, cr8, 3, RWT_REGISTER,      OP_CR,      8, RWF_X64,    P_64,   0)

#endif

/* predefined types. BYTE must be first! */
/* token    str    len  rm_byte     op2        opcode   flags cpu     op1 */

res(BYTE,   byte,   4,  RWT_TYPE,   MT_BYTE,   ST_BYTE,    0, P_86,    0)
res(SBYTE,  sbyte,  5,  RWT_TYPE,   MT_SBYTE,  ST_SBYTE,   0, P_86,    0)
res(WORD,   word,   4,  RWT_TYPE,   MT_WORD,   ST_WORD,    0, P_86,    0)
res(SWORD,  sword,  5,  RWT_TYPE,   MT_SWORD,  ST_SWORD,   0, P_86,    0)
res(DWORD,  dword,  5,  RWT_TYPE,   MT_DWORD,  ST_DWORD,   0, P_86,    0)
res(SDWORD, sdword, 6,  RWT_TYPE,   MT_SDWORD, ST_SDWORD,  0, P_86,    0)
res(REAL4,  real4,  5,  RWT_TYPE,   MT_REAL4,  ST_REAL4,   0, P_86,    0)
res(FWORD,  fword,  5,  RWT_TYPE,   MT_FWORD,  ST_FWORD,   0, P_86,    0)
res(QWORD,  qword,  5,  RWT_TYPE,   MT_QWORD,  ST_QWORD,   0, P_86,    0)
res(SQWORD, sqword, 6,  RWT_TYPE,   MT_SQWORD, ST_SQWORD,  0, P_86,    0)
res(REAL8,  real8,  5,  RWT_TYPE,   MT_REAL8,  ST_REAL8,   0, P_86,    0)
res(TBYTE,  tbyte,  5,  RWT_TYPE,   MT_TBYTE,  ST_TBYTE,   0, P_86,    0)
res(REAL10, real10, 6,  RWT_TYPE,   MT_REAL10, ST_REAL10,  0, P_86,    0)
res(OWORD,  oword,  5,  RWT_TYPE,   MT_OWORD,  ST_OWORD,   0, P_86,    0)
res(NEAR,   near,   4,  RWT_TYPE,   MT_NEAR,   ST_NEAR,    0, P_86,    0)
res(FAR,    far,    3,  RWT_TYPE,   MT_FAR,    ST_FAR,     0, P_86,    0)
res(NEAR16, near16, 6,  RWT_TYPE,   MT_NEAR,   ST_NEAR16,  0, P_386,   0)
res(NEAR32, near32, 6,  RWT_TYPE,   MT_NEAR,   ST_NEAR32,  0, P_386,   0)
res(FAR16,  far16,  5,  RWT_TYPE,   MT_FAR,    ST_FAR16,   0, P_386,   0)
res(FAR32,  far32,  5,  RWT_TYPE,   MT_FAR,    ST_FAR32,   0, P_386,   0)
#if XMMWORD
res(MMWORD, mmword, 6,  RWT_TYPE,   MT_QWORD,  ST_QWORD,   0, P_586|P_MMX, 0)
res(XMMWORD,xmmword,7,  RWT_TYPE,   MT_OWORD,  ST_OWORD,   0, P_686|P_SSE1, 0)
#endif

/* unary operators. field before cpu contains priority */

res(DOT_TYPE,   .type,       5,  RWT_UNARY_OP, AT_ALL,    14,  0,   P_86,  0)
res(HIGH,       high,        4,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  0)
#if LOHI32
res(HIGH32,     high32,      6,  RWT_UNARY_OP, AT_CONST,   6,  0,   P_86,  0)
#endif
res(HIGHWORD,   highword,    8,  RWT_UNARY_OP, AT_CONST,   6,  0,   P_86,  0)
#if IMAGERELSUPP
res(IMAGEREL,   imagerel,    8,  RWT_UNARY_OP, AT_TLN,     5,  0,   P_86,  0)
#endif
res(LENGTH,     length,      6,  RWT_UNARY_OP, AT_LF,      3,  0,   P_86,  0)
res(LENGTHOF,   lengthof,    8,  RWT_UNARY_OP, AT_LF,      3,  0,   P_86,  0)
res(LOW,        low,         3,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  0)
#if LOHI32
res(LOW32,      low32,       5,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  0)
#endif
res(LOWWORD,    lowword,     7,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  0)
res(LROFFSET,   lroffset,    8,  RWT_UNARY_OP, AT_LFN,     5,  0,   P_86,  0)
res(MASK,       mask,        4,  RWT_UNARY_OP, AT_TBF,     3,  0,   P_86,  0)
res(OFFSET,     offset,      6,  RWT_UNARY_OP, AT_TLFN,    5,  0,   P_86,  0)
res(OPATTR,     opattr,      6,  RWT_UNARY_OP, AT_ALL,    14,  0,   P_86,  0)
#if SECTIONRELSUPP
res(SECTIONREL, sectionrel, 10,  RWT_UNARY_OP, AT_LABEL,   5,  0,   P_86,  0)
#endif
res(SEG,        seg,         3,  RWT_UNARY_OP, AT_LABEL,   5,  0,   P_86,  0)
res(SHORT,      short,       5,  RWT_UNARY_OP, AT_LABEL,  14,  0,   P_86,  0)
res(SIZE,       size,        4,  RWT_UNARY_OP, AT_TLF,     3,  0,   P_86,  0)
res(SIZEOF,     sizeof,      6,  RWT_UNARY_OP, AT_TLF,     3,  0,   P_86,  0)
res(THIS,       this,        4,  RWT_UNARY_OP, AT_TYPE,    5,  0,   P_86,  0)
res(TYPE,       type,        4,  RWT_UNARY_OP, AT_ALL,     5,  0,   P_86,  0)
res(WIDTH,      width,       5,  RWT_UNARY_OP, AT_TBF,     3,  0,   P_86,  0)

/* binary operators. field before cpu contains priority */

res(EQ,   eq,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(NE,   ne,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(GE,   ge,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(GT,   gt,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(LE,   le,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(LT,   lt,  2,  RWT_BINARY_OP,       0,     10,  0,  P_86,        0)
res(MOD,  mod, 3,  RWT_BINARY_OP,       0,      8,  0,  P_86,        0)

/* DUP and PTR are also binary operators, but they
 * must be handled differently.
 */

res(DUP,    dup,   3, RWT_RES_ID,  0,   0, 0,   P_86,        0)
res(PTR,    ptr,   3, RWT_RES_ID,  0,   0, 0,   P_86,        0)
res(ADDR,   addr,  4, RWT_RES_ID,  0,   0, 0,   P_86,        0)
res(FLAT,   flat,  4, RWT_RES_ID,  0,   0, 0,   P_86,        0)
res(VARARG, vararg,6, RWT_RES_ID,  0,   0, 0,   P_86,        0)

res(BASIC,    basic,       5, RWT_RES_ID, 0,   LANG_BASIC,       0,      P_86,  0)
res(C,        c,           1, RWT_RES_ID, 0,   LANG_C,           0,      P_86,  0)
res(FORTRAN,  fortran,     7, RWT_RES_ID, 0,   LANG_FORTRAN,     0,      P_86,  0)
res(PASCAL,   pascal,      6, RWT_RES_ID, 0,   LANG_PASCAL,      0,      P_86,  0)
res(STDCALL,  stdcall,     7, RWT_RES_ID, 0,   LANG_STDCALL,     0,      P_86,  0)
res(SYSCALL,  syscall,     7, RWT_RES_ID, 0,   LANG_SYSCALL,     0,      P_86,  0)
res(FASTCALL, fastcall,    8, RWT_RES_ID, 0,   LANG_FASTCALL,    0,      P_86,  0)

/* directives */
/* some directives are ordered in groups with start and end point */
/* if those points change, adjust directive.c! */

/* cpu directives (start: .8086, end: .NO87 */

res(DOT_8086, .8086,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_86  )
res(DOT_186,  .186,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_186 )
res(DOT_286,  .286,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_286 )
res(DOT_286C, .286c,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_286 )
res(DOT_286P, .286p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_286p)
res(DOT_386,  .386,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_386 )
res(DOT_386C, .386c,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_386 )
res(DOT_386P, .386p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_386p)
res(DOT_486,  .486,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_486 )
res(DOT_486P, .486p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_486p)
res(DOT_586,  .586,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_586 )
res(DOT_586P, .586p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_586p)
res(DOT_686,  .686,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_686 )
res(DOT_686P, .686p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_686p)
res(DOT_K3D,  .k3d,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_586, P_K3D | P_MMX)
res(DOT_MMX,  .mmx,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_586, P_MMX )
res(DOT_XMM,  .xmm,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_686, P_MMX | P_SSEALL)
#if AMD64_SUPPORT
res(DOT_X64,  .x64,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_64  )
res(DOT_X64P, .x64p,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_64p )
#endif

res(DOT_8087, .8087,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_87  )
res(DOT_287,  .287,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_287 )
res(DOT_387,  .387,        4,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_387 )
res(DOT_NO87, .no87,       5,  RWT_DIRECTIVE,     0,   0,  0,   P_86,  P_NO87)

/* listing directives (start: .CREF, end: TITLE */
/* .LFCOND is synonym for .LISTIF
 * .SFCOND is synonym for .NOLISTIF
 * .TFCOND toggles .LFCOND, .SFCOND
 * .XALL is synonym for .LISTMACRO
 * .LALL is synonym for .LISTMACROALL
 * .SALL is synonym for .NOLISTMACRO
 */

res(DOT_CREF,         .cref,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LALL,         .lall,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LFCOND,       .lfcond,        7, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LIST,         .list,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LISTALL,      .listall,       8, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LISTIF,       .listif,        7, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LISTMACRO,    .listmacro,    10, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_LISTMACROALL, .listmacroall, 13, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_NOCREF,       .nocref,        7, RWT_DIRECTIVE, DF_NOEXPAND, 0, 0,  P_86, 0)
res(DOT_NOLIST,       .nolist,        7, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_NOLISTIF,     .nolistif,      9, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_NOLISTMACRO,  .nolistmacro,  12, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_SALL,         .sall,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_SFCOND,       .sfcond,        7, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_TFCOND,       .tfcond,        7, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_XALL,         .xall,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(DOT_XCREF,        .xcref,         6, RWT_DIRECTIVE, DF_NOEXPAND, 0, 0,  P_86, 0)
res(DOT_XLIST,        .xlist,         6, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(PAGE,             page,           4, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(SUBTITLE,         subtitle,       8, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(SUBTTL,           subttl,         6, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)
res(TITLE,            title,          5, RWT_DIRECTIVE, 0,           0, 0,  P_86, 0)


res(DOT_ALPHA,     .alpha,      6,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_DOSSEG,    .dosseg,     7,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_SEQ,       .seq,        4,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOSSEG,        dosseg,      6,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)

res(DOT_CODE,       .code,      5,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_CONST,      .const,     6,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_DATA,       .data,      5,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_DATA_UN,    .data?,     6,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_FARDATA,    .fardata,   8,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_FARDATA_UN, .fardata?,  9,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)
res(DOT_STACK,      .stack,     6,  RWT_DIRECTIVE,     0,  0, 0,  P_86, 0)

/* hll directives */

res(DOT_BREAK,      .break,     6,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_CONTINUE,   .continue,  9,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_ELSE,       .else,      5,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_ELSEIF,     .elseif,    7,  RWT_DIRECTIVE,     DF_CEXPR,  0,  0,   P_86, 0)
res(DOT_ENDIF,      .endif,     6,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_ENDW,       .endw,      5,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_IF,         .if,        3,  RWT_DIRECTIVE,     DF_CEXPR,  0,  0,   P_86, 0)
res(DOT_REPEAT,     .repeat,    7,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_UNTIL,      .until,     6,  RWT_DIRECTIVE,     DF_CEXPR,  0,  0,   P_86, 0)
res(DOT_UNTILCXZ,   .untilcxz,  9,  RWT_DIRECTIVE,     DF_CEXPR,  0,  0,   P_86, 0)
res(DOT_WHILE,      .while,     6,  RWT_DIRECTIVE,     DF_CEXPR,  0,  0,   P_86, 0)

res(DOT_EXIT,       .exit,      5,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_STARTUP,    .startup,   8,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)

res(DOT_MODEL,      .model,     6,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)
res(DOT_RADIX,      .radix,     6,  RWT_DIRECTIVE,     0,         0,  0,   P_86, 0)

/* directives invalid for IA32+ */

res(DOT_SAFESEH,    .safeseh,   8,  RWT_DIRECTIVE, 0,     0,  RWF_IA32,   P_386, 0)

/* error directives, handled by preprocessor */

res(DOT_ERR,     .err,         4, RWT_DIRECTIVE, 0,           DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERR1,    .err1,        5, RWT_DIRECTIVE, 0,           DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERR2,    .err2,        5, RWT_DIRECTIVE, 0,           DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRB,    .errb,        5, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRDEF,  .errdef,      7, RWT_DIRECTIVE, DF_NOEXPAND, DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRDIF,  .errdif,      7, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRDIFI, .errdifi,     8, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRE,    .erre,        5, RWT_DIRECTIVE, 0,           DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRIDN,  .erridn,      7, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRIDNI, .erridni,     8, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRNB,   .errnb,       6, RWT_DIRECTIVE, DF_STRPARM,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRNDEF, .errndef,     8, RWT_DIRECTIVE, DF_NOEXPAND, DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERRNZ,   .errnz,       6, RWT_DIRECTIVE, 0,           DRT_ERRDIR,  0, P_86, 0)

/* conditional assembly directives, handled by preprocessor */

res(COMMENT,      comment,     7, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSE,         else,        4, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIF,       elseif,      6, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIF1,      elseif1,     7, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIF2,      elseif2,     7, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFB,      elseifb,     7, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFDEF,    elseifdef,   9, RWT_DIRECTIVE, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFDIF,    elseifdif,   9, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFDIFI,   elseifdifi, 10, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFE,      elseife,     7, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFIDN,    elseifidn,   9, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFIDNI,   elseifidni, 10, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFNB,     elseifnb,    8, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(ELSEIFNDEF,   elseifndef, 10, RWT_DIRECTIVE, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, 0)
res(ENDIF,        endif,       5, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IF,           if,          2, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IF1,          if1,         3, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IF2,          if2,         3, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IFB,          ifb,         3, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFDEF,        ifdef,       5, RWT_DIRECTIVE, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, 0)
res(IFDIF,        ifdif,       5, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFDIFI,       ifdifi,      6, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFE,          ife,         3, RWT_DIRECTIVE, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IFIDN,        ifidn,       5, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFIDNI,       ifidni,      6, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFNB,         ifnb,        4, RWT_DIRECTIVE, DF_STRPARM,  DRT_CONDDIR, 0, P_86, 0)
res(IFNDEF,       ifndef,      6, RWT_DIRECTIVE, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, 0)

/* assembly time loop directives, handled by preprocessor */

res(FOR,          for,         3, RWT_DIRECTIVE, DF_NOEXPAND, DRT_LOOPDIR, 0, P_86, 0)
res(FORC,         forc,        4, RWT_DIRECTIVE, DF_NOEXPAND, DRT_LOOPDIR, 0, P_86, 0)
res(IRP,          irp,         3, RWT_DIRECTIVE, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(IRPC,         irpc,        4, RWT_DIRECTIVE, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(REPEAT,       repeat,      6, RWT_DIRECTIVE, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(REPT,         rept,        4, RWT_DIRECTIVE, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(WHILE,        while,       5, RWT_DIRECTIVE, 0,           DRT_LOOPDIR, 0, P_86, 0)

/* other preprocessor directives */

res(MACRO,     macro,          5, RWT_DIRECTIVE, DF_LABEL,              0, 0, P_86, 0)
res(EXITM,     exitm,          5, RWT_DIRECTIVE, DF_STRPARM,            0, 0, P_86, 0)
res(GOTO,      goto,           4, RWT_DIRECTIVE, 0,                     0, 0, P_86, 0)
res(INCLUDE,   include,        7, RWT_DIRECTIVE, DF_NOEXPAND, DRT_INCLUDE, 0, P_86, 0)

res(CATSTR,    catstr,         6, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL, 0, 0, P_86, 0)
res(INSTR,     instr,          5, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL, 0, 0, P_86, 0)
res(SIZESTR,   sizestr,        7, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL, 0, 0, P_86, 0)
res(SUBSTR,    substr,         6, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL, 0, 0, P_86, 0)

res(DB,        db,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_BYTE  )
res(DD,        dd,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_DWORD )
res(DF,        df,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_FWORD )
res(DQ,        dq,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_QWORD )
res(DT,        dt,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_TBYTE )
res(DW,        dw,             2, RWT_DIRECTIVE, DF_LABEL,    DRT_DATADIR, 0, P_86, ST_WORD  )

#if AMD64_SUPPORT
res(DOT_ALLOCSTACK, .allocstack,11, RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_ENDPROLOG,  .endprolog, 10, RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_PUSHFRAME,  .pushframe, 10, RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_PUSHREG,    .pushreg,   8,  RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_SAVEREG,    .savereg,   8,  RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_SAVEXMM128, .savexmm128,11, RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(DOT_SETFRAME,   .setframe,  9,  RWT_DIRECTIVE, 0,        0, RWF_X64,  P_64, 0)
res(FRAME,          frame,      5,  RWT_RES_ID,    0,        0, RWF_X64,  P_64, 0)
#endif

/* other directives */

res(ALIAS,       alias,       5, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ALIGN,       align,       5, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ASSUME,      assume,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(COMM,        comm,        4, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ECHO,        echo,        4, RWT_DIRECTIVE, DF_NOEXPAND,          0, 0,  P_86, 0)
res(END,         end,         3, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ENDM,        endm,        4, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ENDP,        endp,        4, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(ENDS,        ends,        4, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(EQU,         equ,         3, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL,0, 0,  P_86, 0)
res(EVEN,        even,        4, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(EXTERN,      extern,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(EXTERNDEF,   externdef,   9, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(EXTRN,       extrn,       5, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(GROUP,       group,       5, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
#if INCLUDEBIN
res(INCBIN,      incbin,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
#endif
res(INCLUDELIB,  includelib, 10, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(INVOKE,      invoke,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(LABEL,       label,       5, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(LOCAL,       local,       5, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(NAME,        name,        4, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(OPTION,      option,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(ORG,         org,         3, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(POPCONTEXT,  popcontext, 10, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(PROC,        proc,        4, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(PROTO,       proto,       5, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(PUBLIC,      public,      6, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(PURGE,       purge,       5, RWT_DIRECTIVE, DF_NOEXPAND,          0, 0,  P_86, 0)
res(PUSHCONTEXT, pushcontext,11, RWT_DIRECTIVE, 0,                    0, 0,  P_86, 0)
res(RECORD,      record,      6, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(SEGMENT,     segment,     7, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(STRUC,       struc,       5, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(STRUCT,      struct,      6, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(TEXTEQU,     textequ,     7, RWT_DIRECTIVE, DF_STRPARM | DF_LABEL,0, 0,  P_86, 0)
res(TYPEDEF,     typedef,     7, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)
res(UNION,       union,       5, RWT_DIRECTIVE, DF_LABEL,             0, 0,  P_86, 0)

