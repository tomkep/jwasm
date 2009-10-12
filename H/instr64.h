/****************************************************************************
*
* Description:  instructions which are to be replaced in long-mode
*
****************************************************************************/

/*   tok   (suffix)             op1      byte1_info  op2            op3 opnd_dir rm_info opcode    rm_byte     cpu      prefix */

#if AMD64_SUPPORT
ins (CALL, call, 4,             OP_I32,     0,       OP_NONE,       OP3_NONE,0,  no_RM,  0xE8,     0x00,       P_64,        0)
insn(CALL, 1,                   OP_R64,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x10,       P_64,        0)
insn(CALL, 2,                   OP_M64,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x10,       P_64,        0)
/* FAR call */
insm(CALL, 3,                   OP_M32,     F_16,    OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x18,       P_64,        0)
insn(CALL, 4,                   OP_M48,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x18,       P_64,        0)
insn(CALL, 5,                   OP_M80,     F_48,    OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x18,       P_64,        0)
ins (JMP, jmp, 3,               OP_I8,      0,       OP_NONE,       OP3_NONE,0,  no_RM,  0xEB,     0x00,       P_86,        0)
insn(JMP, 1,                    OP_I32,     0,       OP_NONE,       OP3_NONE,0,  no_RM,  0xE9,     0x00,       P_64,        0)
insn(JMP, 2,                    OP_R64,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x20,       P_64,        0)
insn(JMP, 3,                    OP_M64,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x20,       P_64,        0)
/* FAR jmp */
insm(JMP, 4,                    OP_M32,     F_16,    OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x28,       P_64,        0)
insn(JMP, 5,                    OP_M48,     0,       OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x28,       P_64,        0)
insn(JMP, 6,                    OP_M80,     F_48,    OP_NONE,       OP3_NONE,0,  no_WDS, 0xFF,     0x28,       P_64,        0)
ins (LGDT, lgdt, 4,             OP_M80,     F_0F,    OP_NONE,       OP3_NONE,0,  no_WDS, 0x01,     0x10,       P_64,        0)
ins (LIDT, lidt, 4,             OP_M80,     F_0F,    OP_NONE,       OP3_NONE,0,  no_WDS, 0x01,     0x18,       P_64,        0)
#endif
