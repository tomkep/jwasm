/****************************************************************************
*
* Description:  instructions which are to be replaced in long-mode
*
****************************************************************************/

/*   tok   (suffix)            op1      byte1_info  op2            op3 opnd_dir rm_info opcode    rm_byte     cpu      prefix */

#if AMD64_SUPPORT
ins (LGDT, lgdt, 4,            OP_M_TB,     F_0F,   OP_NONE,       OP3_NONE,0,  no_WDS, 0x01,     0x10,       P_64,      0)
ins (LIDT, lidt, 4,            OP_M_TB,     F_0F,   OP_NONE,       OP3_NONE,0,  no_WDS, 0x01,     0x18,       P_64,      0)
#endif
