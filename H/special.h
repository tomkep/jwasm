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
*               for directives see directve.h!
*               for instructions see instruct.h!
*
****************************************************************************/

/* v1.96: items needn't be sorted anymore!
 * The items are stored in structures of type asm_special.
 * If an item is inserted, moved or deleted, the project needs
 * a full rebuild.
 */

/* token str len   type        value    value8    flags  cpu     sflags */

/* registers AH-BH must be consecutive, start with AH and end with BH */

res(AL,   al, 2, RWT_REG,      OP_AL,      0,       0,   P_86,    0)
res(CL,   cl, 2, RWT_REG,      OP_CL,      1,       0,   P_86,    0)
res(DL,   dl, 2, RWT_REG,      OP_R8,      2,       0,   P_86,    0)
res(BL,   bl, 2, RWT_REG,      OP_R8,      3,       0,   P_86,    0)
res(AH,   ah, 2, RWT_REG,      OP_R8,      4,       0,   P_86,    0)
res(CH,   ch, 2, RWT_REG,      OP_R8,      5,       0,   P_86,    0)
res(DH,   dh, 2, RWT_REG,      OP_R8,      6,       0,   P_86,    0)
res(BH,   bh, 2, RWT_REG,      OP_R8,      7,       0,   P_86,    0)

res(AX,   ax, 2, RWT_REG,      OP_AX,      0,       0,   P_86,    0)
res(CX,   cx, 2, RWT_REG,      OP_R16,     1,       0,   P_86,    0)
res(DX,   dx, 2, RWT_REG,      OP_DX,      2,       0,   P_86,    0)
res(BX,   bx, 2, RWT_REG,      OP_R16,     3,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(SP,   sp, 2, RWT_REG,      OP_R16,     4,       0,   P_86,    0)
res(BP,   bp, 2, RWT_REG,      OP_R16,     5,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(SI,   si, 2, RWT_REG,      OP_R16,     6,       0,   P_86,    SFR_IREG|SFR_SIZ2)
res(DI,   di, 2, RWT_REG,      OP_R16,     7,       0,   P_86,    SFR_IREG|SFR_SIZ2)

res(EAX, eax, 3, RWT_REG,      OP_EAX,     0,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ECX, ecx, 3, RWT_REG,      OP_R32,     1,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EDX, edx, 3, RWT_REG,      OP_R32,     2,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EBX, ebx, 3, RWT_REG,      OP_R32,     3,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ESP, esp, 3, RWT_REG,      OP_R32,     4,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EBP, ebp, 3, RWT_REG,      OP_R32,     5,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(ESI, esi, 3, RWT_REG,      OP_R32,     6,       0,   P_386,   SFR_IREG|SFR_SIZ4)
res(EDI, edi, 3, RWT_REG,      OP_R32,     7,       0,   P_386,   SFR_IREG|SFR_SIZ4)

res(ES,   es, 2, RWT_REG,      OP_SR86,    0,       0,   P_86,    0)
res(CS,   cs, 2, RWT_REG,      OP_SR86,    1,       0,   P_86,    0)
res(SS,   ss, 2, RWT_REG,      OP_SR86,    2,       0,   P_86,    0)
res(DS,   ds, 2, RWT_REG,      OP_SR86,    3,       0,   P_86,    0)
res(FS,   fs, 2, RWT_REG,      OP_SR386,   4,       0,   P_386,   0)
res(GS,   gs, 2, RWT_REG,      OP_SR386,   5,       0,   P_386,   0)

res(ST,   st, 2, RWT_REG,      OP_ST,      0,       0,   P_87,    0)

res(MM0, mm0, 3, RWT_REG,      OP_MMX,     0,       0,   P_MMX,   0)
res(MM1, mm1, 3, RWT_REG,      OP_MMX,     1,       0,   P_MMX,   0)
res(MM2, mm2, 3, RWT_REG,      OP_MMX,     2,       0,   P_MMX,   0)
res(MM3, mm3, 3, RWT_REG,      OP_MMX,     3,       0,   P_MMX,   0)
res(MM4, mm4, 3, RWT_REG,      OP_MMX,     4,       0,   P_MMX,   0)
res(MM5, mm5, 3, RWT_REG,      OP_MMX,     5,       0,   P_MMX,   0)
res(MM6, mm6, 3, RWT_REG,      OP_MMX,     6,       0,   P_MMX,   0)
res(MM7, mm7, 3, RWT_REG,      OP_MMX,     7,       0,   P_MMX,   0)

res(XMM0, xmm0, 4, RWT_REG,    OP_XMM,     0,       0,   P_SSE1,  0)
res(XMM1, xmm1, 4, RWT_REG,    OP_XMM,     1,       0,   P_SSE1,  0)
res(XMM2, xmm2, 4, RWT_REG,    OP_XMM,     2,       0,   P_SSE1,  0)
res(XMM3, xmm3, 4, RWT_REG,    OP_XMM,     3,       0,   P_SSE1,  0)
res(XMM4, xmm4, 4, RWT_REG,    OP_XMM,     4,       0,   P_SSE1,  0)
res(XMM5, xmm5, 4, RWT_REG,    OP_XMM,     5,       0,   P_SSE1,  0)
res(XMM6, xmm6, 4, RWT_REG,    OP_XMM,     6,       0,   P_SSE1,  0)
res(XMM7, xmm7, 4, RWT_REG,    OP_XMM,     7,       0,   P_SSE1,  0)

res(CR0, cr0, 3, RWT_REG,      OP_SPECREG, 0,       0,   P_386,   0)
res(CR2, cr2, 3, RWT_REG,      OP_SPECREG, 2,       0,   P_386,   0)
res(CR3, cr3, 3, RWT_REG,      OP_SPECREG, 3,       0,   P_386,   0)
res(CR4, cr4, 3, RWT_REG,      OP_SPECREG, 4,       0,   P_586,   0)
res(DR0, dr0, 3, RWT_REG,      OP_SPECREG, 0x10,    0,   P_386,   0)
res(DR1, dr1, 3, RWT_REG,      OP_SPECREG, 0x11,    0,   P_386,   0)
res(DR2, dr2, 3, RWT_REG,      OP_SPECREG, 0x12,    0,   P_386,   0)
res(DR3, dr3, 3, RWT_REG,      OP_SPECREG, 0x13,    0,   P_386,   0)
res(DR6, dr6, 3, RWT_REG,      OP_SPECREG, 0x16,    0,   P_386,   0)
res(DR7, dr7, 3, RWT_REG,      OP_SPECREG, 0x17,    0,   P_386,   0)
res(TR3, tr3, 3, RWT_REG,      OP_SPECREG, 0x23,    0,   P_486,   0)
res(TR4, tr4, 3, RWT_REG,      OP_SPECREG, 0x24,    0,   P_486,   0)
res(TR5, tr5, 3, RWT_REG,      OP_SPECREG, 0x25,    0,   P_486,   0)
res(TR6, tr6, 3, RWT_REG,      OP_SPECREG, 0x26,    0,   P_386,   0)
res(TR7, tr7, 3, RWT_REG,      OP_SPECREG, 0x27,    0,   P_386,   0)

#if AMD64_SUPPORT

/* for simplicity, all x64 reserved words must be consecutive
 * ( see Set64Bit() in parser.c ).
 */

res(SPL, spl, 3, RWT_REG,      OP_R8,      4, RWF_X64,    P_64,   0)
res(BPL, bpl, 3, RWT_REG,      OP_R8,      5, RWF_X64,    P_64,   0)
res(SIL, sil, 3, RWT_REG,      OP_R8,      6, RWF_X64,    P_64,   0)
res(DIL, dil, 3, RWT_REG,      OP_R8,      7, RWF_X64,    P_64,   0)
res(R8B, r8b, 3, RWT_REG,      OP_R8,      8, RWF_X64,    P_64,   0)
res(R9B, r9b, 3, RWT_REG,      OP_R8,      9, RWF_X64,    P_64,   0)
res(R10B,r10b,4, RWT_REG,      OP_R8,     10, RWF_X64,    P_64,   0)
res(R11B,r11b,4, RWT_REG,      OP_R8,     11, RWF_X64,    P_64,   0)
res(R12B,r12b,4, RWT_REG,      OP_R8,     12, RWF_X64,    P_64,   0)
res(R13B,r13b,4, RWT_REG,      OP_R8,     13, RWF_X64,    P_64,   0)
res(R14B,r14b,4, RWT_REG,      OP_R8,     14, RWF_X64,    P_64,   0)
res(R15B,r15b,4, RWT_REG,      OP_R8,     15, RWF_X64,    P_64,   0)

res(R8W, r8w, 3, RWT_REG,      OP_R16,     8, RWF_X64,    P_64,   0)
res(R9W, r9w, 3, RWT_REG,      OP_R16,     9, RWF_X64,    P_64,   0)
res(R10W,r10w,4, RWT_REG,      OP_R16,    10, RWF_X64,    P_64,   0)
res(R11W,r11w,4, RWT_REG,      OP_R16,    11, RWF_X64,    P_64,   0)
res(R12W,r12w,4, RWT_REG,      OP_R16,    12, RWF_X64,    P_64,   0)
res(R13W,r13w,4, RWT_REG,      OP_R16,    13, RWF_X64,    P_64,   0)
res(R14W,r14w,4, RWT_REG,      OP_R16,    14, RWF_X64,    P_64,   0)
res(R15W,r15w,4, RWT_REG,      OP_R16,    15, RWF_X64,    P_64,   0)

res(R8D, r8d, 3, RWT_REG,      OP_R32,     8, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R9D, r9d, 3, RWT_REG,      OP_R32,     9, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R10D,r10d,4, RWT_REG,      OP_R32,    10, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R11D,r11d,4, RWT_REG,      OP_R32,    11, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R12D,r12d,4, RWT_REG,      OP_R32,    12, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R13D,r13d,4, RWT_REG,      OP_R32,    13, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R14D,r14d,4, RWT_REG,      OP_R32,    14, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)
res(R15D,r15d,4, RWT_REG,      OP_R32,    15, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ4)

res(RAX, rax, 3, RWT_REG,      OP_RAX,     0, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RCX, rcx, 3, RWT_REG,      OP_R64,     1, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RDX, rdx, 3, RWT_REG,      OP_R64,     2, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RBX, rbx, 3, RWT_REG,      OP_R64,     3, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RSP, rsp, 3, RWT_REG,      OP_R64,     4, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RBP, rbp, 3, RWT_REG,      OP_R64,     5, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RSI, rsi, 3, RWT_REG,      OP_R64,     6, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(RDI, rdi, 3, RWT_REG,      OP_R64,     7, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R8,  r8,  2, RWT_REG,      OP_R64,     8, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R9,  r9,  2, RWT_REG,      OP_R64,     9, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R10, r10, 3, RWT_REG,      OP_R64,    10, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R11, r11, 3, RWT_REG,      OP_R64,    11, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R12, r12, 3, RWT_REG,      OP_R64,    12, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R13, r13, 3, RWT_REG,      OP_R64,    13, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R14, r14, 3, RWT_REG,      OP_R64,    14, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)
res(R15, r15, 3, RWT_REG,      OP_R64,    15, RWF_X64,    P_64,   SFR_IREG|SFR_SIZ8)

res(XMM8, xmm8, 4, RWT_REG,    OP_XMM,     8, RWF_X64,    P_64,   0)
res(XMM9, xmm9, 4, RWT_REG,    OP_XMM,     9, RWF_X64,    P_64,   0)
res(XMM10,xmm10,5, RWT_REG,    OP_XMM,    10, RWF_X64,    P_64,   0)
res(XMM11,xmm11,5, RWT_REG,    OP_XMM,    11, RWF_X64,    P_64,   0)
res(XMM12,xmm12,5, RWT_REG,    OP_XMM,    12, RWF_X64,    P_64,   0)
res(XMM13,xmm13,5, RWT_REG,    OP_XMM,    13, RWF_X64,    P_64,   0)
res(XMM14,xmm14,5, RWT_REG,    OP_XMM,    14, RWF_X64,    P_64,   0)
res(XMM15,xmm15,5, RWT_REG,    OP_XMM,    15, RWF_X64,    P_64,   0)

res(CR8, cr8, 3, RWT_REG,      OP_SPECREG, 8, RWF_X64,    P_64,   0)

#endif

/* predefined types. BYTE must be first! */
/* token    str    len  type        value      value8   flags cpu     sflags */

res(BYTE,   byte,   4,  RWT_STYPE,  MT_BYTE,   ST_BYTE,    0, P_86,    0)
res(SBYTE,  sbyte,  5,  RWT_STYPE,  MT_SBYTE,  ST_SBYTE,   0, P_86,    0)
res(WORD,   word,   4,  RWT_STYPE,  MT_WORD,   ST_WORD,    0, P_86,    0)
res(SWORD,  sword,  5,  RWT_STYPE,  MT_SWORD,  ST_SWORD,   0, P_86,    0)
res(DWORD,  dword,  5,  RWT_STYPE,  MT_DWORD,  ST_DWORD,   0, P_86,    0)
res(SDWORD, sdword, 6,  RWT_STYPE,  MT_SDWORD, ST_SDWORD,  0, P_86,    0)
res(REAL4,  real4,  5,  RWT_STYPE,  MT_REAL4,  ST_REAL4,   0, P_86,    0)
res(FWORD,  fword,  5,  RWT_STYPE,  MT_FWORD,  ST_FWORD,   0, P_86,    0)
res(QWORD,  qword,  5,  RWT_STYPE,  MT_QWORD,  ST_QWORD,   0, P_86,    0)
res(SQWORD, sqword, 6,  RWT_STYPE,  MT_SQWORD, ST_SQWORD,  0, P_86,    0)
res(REAL8,  real8,  5,  RWT_STYPE,  MT_REAL8,  ST_REAL8,   0, P_86,    0)
res(TBYTE,  tbyte,  5,  RWT_STYPE,  MT_TBYTE,  ST_TBYTE,   0, P_86,    0)
res(REAL10, real10, 6,  RWT_STYPE,  MT_REAL10, ST_REAL10,  0, P_86,    0)
res(OWORD,  oword,  5,  RWT_STYPE,  MT_OWORD,  ST_OWORD,   0, P_86,    0)
/* NEAR must be first, FAR32 must be last, all contiguous */
res(NEAR,   near,   4,  RWT_STYPE,  MT_NEAR,   ST_NEAR,    0, P_86,    0)
res(FAR,    far,    3,  RWT_STYPE,  MT_FAR,    ST_FAR,     0, P_86,    0)
res(NEAR16, near16, 6,  RWT_STYPE,  MT_NEAR,   ST_NEAR16,  0, P_386,   0)
res(NEAR32, near32, 6,  RWT_STYPE,  MT_NEAR,   ST_NEAR32,  0, P_386,   0)
res(FAR16,  far16,  5,  RWT_STYPE,  MT_FAR,    ST_FAR16,   0, P_386,   0)
res(FAR32,  far32,  5,  RWT_STYPE,  MT_FAR,    ST_FAR32,   0, P_386,   0)
#if XMMWORD
res(MMWORD, mmword, 6,  RWT_STYPE,  MT_QWORD,  ST_QWORD,   0, P_586|P_MMX, 0)
res(XMMWORD,xmmword,7,  RWT_STYPE,  MT_OWORD,  ST_OWORD,   0, P_686|P_SSE1, 0)
#endif

/* unary operators. value8 contains priority */
/* token        str         len  type          value   value8 flags cpu  sflags */
res(DOT_TYPE,   .type,       5,  RWT_UNARY_OP, AT_ALL,    14,  0,   P_86,  UOT_DOT_TYPE  )
res(HIGH,       high,        4,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  UOT_HIGH      )
#if LOHI32
res(HIGH32,     high32,      6,  RWT_UNARY_OP, AT_CONST,   6,  0,   P_86,  UOT_HIGH32    )
#endif
res(HIGHWORD,   highword,    8,  RWT_UNARY_OP, AT_CONST,   6,  0,   P_86,  UOT_HIGHWORD  )
#if IMAGERELSUPP
res(IMAGEREL,   imagerel,    8,  RWT_UNARY_OP, AT_TLN,     5,  0,   P_86,  UOT_IMAGEREL  )
#endif
res(LENGTH,     length,      6,  RWT_UNARY_OP, AT_LF,      3,  0,   P_86,  UOT_LENGTH    )
res(LENGTHOF,   lengthof,    8,  RWT_UNARY_OP, AT_LF,      3,  0,   P_86,  UOT_LENGTHOF  )
res(LOW,        low,         3,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  UOT_LOW       )
#if LOHI32
res(LOW32,      low32,       5,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  UOT_LOW32     )
#endif
res(LOWWORD,    lowword,     7,  RWT_UNARY_OP, AT_TLN,     6,  0,   P_86,  UOT_LOWWORD   )
res(LROFFSET,   lroffset,    8,  RWT_UNARY_OP, AT_LFN,     5,  0,   P_86,  UOT_LROFFSET  )
res(MASK,       mask,        4,  RWT_UNARY_OP, AT_TBF,     3,  0,   P_86,  UOT_MASK      )
res(OFFSET,     offset,      6,  RWT_UNARY_OP, AT_TLFN,    5,  0,   P_86,  UOT_OFFSET    )
res(OPATTR,     opattr,      6,  RWT_UNARY_OP, AT_ALL,    14,  0,   P_86,  UOT_OPATTR    )
#if SECTIONRELSUPP
res(SECTIONREL, sectionrel, 10,  RWT_UNARY_OP, AT_LABEL,   5,  0,   P_86,  UOT_SECTIONREL)
#endif
res(SEG,        seg,         3,  RWT_UNARY_OP, AT_LABEL,   5,  0,   P_86,  UOT_SEG       )
res(SHORT,      short,       5,  RWT_UNARY_OP, AT_LABEL,  14,  0,   P_86,  UOT_SHORT     )
res(SIZE,       size,        4,  RWT_UNARY_OP, AT_TLF,     3,  0,   P_86,  UOT_SIZE      )
res(SIZEOF,     sizeof,      6,  RWT_UNARY_OP, AT_TLF,     3,  0,   P_86,  UOT_SIZEOF    )
res(THIS,       this,        4,  RWT_UNARY_OP, AT_TYPE,    5,  0,   P_86,  UOT_THIS      )
res(TYPE,       type,        4,  RWT_UNARY_OP, AT_ALL,     5,  0,   P_86,  UOT_TYPE      )
res(WIDTH,      width,       5,  RWT_UNARY_OP, AT_TBF,     3,  0,   P_86,  UOT_WIDTH     )

/* binary operators. value8 contains priority */

/* token  str len  type             value  value8 flags cpu      sflags */
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
#if AMD64_SUPPORT
res(FRAME,  frame, 5, RWT_RES_ID,  0,   0, RWF_X64, P_64,    0)
#endif

/* languages, must be in this order! */
/* token      str        len  type       value value8        flags       cpu   sflags */
res(C,        c,           1, RWT_RES_ID, 0,   LANG_C,           0,      P_86,  0)
res(SYSCALL,  syscall,     7, RWT_RES_ID, 0,   LANG_SYSCALL,     0,      P_86,  0)
res(STDCALL,  stdcall,     7, RWT_RES_ID, 0,   LANG_STDCALL,     0,      P_86,  0)
res(PASCAL,   pascal,      6, RWT_RES_ID, 0,   LANG_PASCAL,      0,      P_86,  0)
res(FORTRAN,  fortran,     7, RWT_RES_ID, 0,   LANG_FORTRAN,     0,      P_86,  0)
res(BASIC,    basic,       5, RWT_RES_ID, 0,   LANG_BASIC,       0,      P_86,  0)
res(FASTCALL, fastcall,    8, RWT_RES_ID, 0,   LANG_FASTCALL,    0,      P_86,  0)

