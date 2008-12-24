
// message texts
// if a new id is inserted, rebuild everything!

pick( MSG_USAGE,
      "usage: JWasm [ options ] filelist [@env_var]\n"
      "Run \"JWasm -?\" or \"JWasm -h\" for more info\n" ,
      "usage: JWasm [ options ] filelist [@env_var]\n"
      "Run \"JWasm -?\" or \"JWasm -h\" for more info\n" )
pick( MSG_ASSEMBLY_RESULTS,
      "%s: %u lines, %u passes, %u ms, %u warnings, %u errors\n" ,
      "%s: %u lines, %u passes, %u ms, %u warnings, %u errors\n" )
pick( MSG_ERROR_PREFIX,
      "Error!",
      "Error!" )
pick( MSG_WARNING_PREFIX,
      "Warning!",
      "Warning!" )
pick( MSG_FATAL_PREFIX,
      "Fatal!",
      "Fatal!" )
pick( MSG_JWASM,
      "JWasm v%s, %s",
      "JWasm v%s, %s" )
pick( MSG_BANNER,
      "%s, Masm-compatible assembler.\n"
      "Portions Copyright (c) 1992-2002 Sybase, Inc. All Rights Reserved.\n"
      "Source code is available under the Sybase Open Watcom Public License.\n\n" ,
      "JWasm v%s, %s, Masm-compatible assembler.\n"
      "Portions Copyright (c) 1992-2002 Sybase, Inc. All Rights Reserved.\n"
      "Source code is available under the Sybase Open Watcom Public License.\n\n" )
pick( LOCK_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "LOCK prefix is not allowed on this instruction" ,
      "���̖��߂ɑ΂��� LOCK �v���t�B�b�N�X�͎g�p�ł��܂���" )
pick( REP_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "REP prefix is not allowed on this instruction" ,
      "���̖��߂ɑ΂��� REP �v���t�B�b�N�X�͎g�p�ł��܂���" )
pick( INVALID_MEMORY_POINTER,
      "Invalid memory pointer" ,
      "�����ȃ������|�C���^�ł�" )
pick( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING,
      "Invalid instruction with current CPU setting" ,
      "���݂�CPU�̐ݒ�ɑ΂��Ė����Ȗ��߂ł�" )
pick( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE,
      "Instruction or register not accepted in current CPU mode" ,
      "Instruction or register not accepted in current CPU mode" )
pick( INVALID_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING,
      "invalid addressing mode with current CPU setting" ,
      "���݂�CPU�̐ݒ�ɑ΂��Ė����ȃA�h���X���[�h�̎w��ł�" )
pick( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING,
      "Cannot use TR%u-TR%u with current CPU setting" ,
      "Cannot use TR%u-TR%u with current CPU setting" )
#if 0
pick( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 addressing mode with current CPU setting" ,
      "���݂�CPU�̐ݒ�ł�386�A�h���X���g�p�ł��܂���" )
pick( CANNOT_USE_386_SEGMENT_REGISTER_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 segment register with current CPU setting" ,
      "���݂�CPU�̐ݒ�ł�386�Z�O�����g���W�X�^�͎g�p�ł��܂���" )
pick( CANNOT_USE_386_REGISTER_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 register with current CPU setting" ,
      "���݂�CPU�̐ݒ�ł�386���W�X�^�͎g�p�ł��܂���" )
#endif
pick( TOO_MANY_BASE_REGISTERS,
      "Too many base registers" ,
      "�x�[�X���W�X�^���������܂�" )
pick( INVALID_INDEX_REGISTER,
      "Invalid index register" ,
      "�C���f�b�N�X���W�^�������ł�" )
pick( SCALE_FACTOR_MUST_BE_1_2_4_OR_8,
      "Scale factor must be 1, 2, 4 or 8" ,
      "�X�P�[���t�@�N�^��1,2,4,8�̂����ꂩ�łȂ���΂Ȃ�܂���" )
pick( ESP_CANNOT_BE_USED_AS_INDEX,
      "ESP cannot be used as index" ,
      "ESP �̓C���f�b�N�X�Ƃ��Ă͎g�p�ł��܂���" )
pick( TOO_MANY_BASE_INDEX_REGISTERS,
      "Too many base/index registers" ,
      "�x�[�X�^�C���f�b�N�X ���W�X�^���������܂�" )
pick( CANNOT_MIX_16_AND_32_BIT_REGISTERS,
      "Cannot mix 16 and 32-bit registers" ,
      "16�r�b�g���W�X�^��32�r�b�g���W�X�^�͍��݂ł��܂���" )
pick( UNKNOWN_DIRECTIVE,
      "Unknown directive: %s" ,
      "�����ȃf�B���N�e�B�u�ł�" )
pick( EXPECTING_COMMA,
      "Expecting comma" ,
      "�R���}���K�v�ł�" )
pick( EXPECTING_NUMBER,
      "Expecting number" ,
      "�������K�v�ł�" )
pick( INVALID_LABEL_DEFINITION,
      "Invalid label definition" ,
      "�����ȃ��x����`�ł�" )
pick( POP_CS_IS_NOT_ALLOWED,
      "POP CS is not allowed" ,
      "POP CS�͂ł��܂���" )
pick( ONLY_MOV_CAN_USE_SPECIAL_REGISTER,
      "Only MOV can use special register" ,
      "���ʃ��W�X�^�� MOV ���߂ł̂ݎg�p�ł��܂�" )
pick( CANNOT_USE_SHORT_WITH_CALL,
      "Cannot use SHORT with CALL" ,
      "CALL���߂Ƌ��� SHORT ���g�����Ƃ͂ł��܂���" )
pick( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED,
      "Only SHORT displacement is allowed" ,
      "SHORT�f�B�X�v���[�X�����g�̂݉\�ł�" )
pick( SYNTAX_ERROR,
      "Syntax error" ,
      "�\���G���[" )
pick( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION,
      "Prefix must be followed by an instruction" ,
      "�v���t�B�b�N�X�̌�ɂ͖��߂��Ȃ���΂Ȃ�܂���" )
pick( INVALID_IMUL_FORMAT,
      "Invalid IMUL format" ,
      "IMUL���߂̎g�p�`�����s�K�؂ł�" )
pick( INVALID_SHLD_SHRD_FORMAT,
      "Invalid SHLD/SHRD format" ,
      "SHLD/SHRD���߂̎g�p�`�����s�K�؂ł�" )
pick( TOO_MANY_COMMAS,
      "Too many commas" ,
      "�R���}(,)���������܂�" )
pick( SYNTAX_ERROR_UNEXPECTED_COLON,
      "Syntax error: Unexpected colon" ,
      "�\���G���[: �s�K�؂ȃR����(:)������܂�" )
pick( OPERANDS_MUST_BE_THE_SAME_SIZE,
      "Operands must be the same size: %u - %u" ,
      "�I�y�����h�͓����T�C�Y�łȂ���΂Ȃ�܂���" )
pick( INVALID_INSTRUCTION_OPERANDS,
      "Invalid instruction operands" ,
      "�����Ȗ��߃I�y�����h�ł�" )
pick( IMMEDIATE_CONSTANT_TOO_LARGE,
      "Immediate constant too large" ,
      "���l�萔���傫�����܂�" )
pick( IMMEDIATE_DATA_TOO_BIG,
      "Immediate data too large" ,
      "���l�f�[�^���傫�����܂�" )
pick( IMMEDIATE_DATA_OUT_OF_RANGE,
      "Immediate data out of range" ,
      "���l�f�[�^�̒l���͈͊O�ł�" )
pick( CANNOT_USE_SHORT_OR_NEAR,
      "Can not use short or near modifiers with this instruction" ,
      "���̖��߂ƈꏏ��short�܂���near���g�p���邱�Ƃ͂ł��܂���" )
pick( JUMP_OUT_OF_RANGE,
      "Jump out of range" ,
      "�͈͊O�ւ̃W�����v�ł�" )
pick( DISPLACEMENT_OUT_OF_RANGE,
      "Displacement cannot be larger than 32k" ,
      "�f�B�X�v���[�X�����g��32K���z���Ă��܂�" )
pick( INITIALIZER_OUT_OF_RANGE,
      "Initializer value too large" ,
      "�ݒ肳��Ă��鏉���l���傫�����܂�" )
pick( SYMBOL_ALREADY_DEFINED,
      "Symbol already defined: %s" ,
      "Symbol already defined: %s" )
pick( NO_JUMP_TO_AUTO,
      "Can not transfer control to stack symbol" ,
      "�X�^�b�N�V���{���֐�����ڂ����Ƃ͂ł��܂���" )
pick( OFFSET_MAGNITUDE_TOO_LARGE,
      "Offset magnitude too large for specified size" ,
      "�I�t�Z�b�g�̓��[�h(2�o�C�g)��菬�����ł��܂���" )
#if 0
pick( CANNOT_OFFSET_AUTO,
      "Can not take offset of stack symbol" ,
      "�X�^�b�N�V���{���̃I�t�Z�b�g����邱�Ƃ͂ł��܂���" )
#endif
pick( MAGNITUDE_OF_OFFSET_EXCEEDS_16BIT,
      "Magnitude of offset exceeds 16 bit" ,
      "�I�t�Z�b�g��32K���傫���ł��܂���" )
pick( OP2_TOO_BIG,
      "Operand 2 too big" ,
      "�I�y�����h2���傫�����܂�" )
pick( OP1_TOO_SMALL,
      "Operand 1 too small" ,
      "�I�y�����h1�����������܂�" )
pick( INVALID_NUMBER_DIGIT,
      "Invalid number digit" ,
      "�s�K�؂Ȑ��l�ł�" )
pick( LINE_TOO_LONG,
      "Line too long" ,
      "�A�Z���u���R�[�h���������܂�" )
pick( TOO_MANY_TOKENS,
      "Too many tokens in a line" ,
      "1�s�ɂ���g�[�N�����������܂�" )
pick( OPERATOR_EXPECTED,
      "Operator is expected" ,
      "���Z�q���Ȃ���΂Ȃ�܂���" )
pick( OPERAND_EXPECTED,
      "Operand is expected" ,
      "�I�y�����h�łȂ���΂Ȃ�܂���" )
pick( CONSTANT_EXPECTED,
      "Constant expected" ,
      "Constant expected" )
pick( CONSTANT_OPERAND_EXPECTED,
      "Constant operand is expected" ,
      "�萔�I�y�����h�łȂ���΂Ȃ�܂���" )
pick( POSITIVE_SIGN_CONSTANT_EXPECTED,
      "A constant operand is expected after a positive sign" ,
      "���̕���(+)�̌�ɂ͒萔�I�y�����h���Ȃ���΂Ȃ�܂���" )
pick( NEGATIVE_SIGN_CONSTANT_EXPECTED,
      "A constant operand is expected after a negative sign" ,
      "���̕���(-)�̌�ɂ͒萔�I�y�����h���Ȃ���΂Ȃ�܂���" )
pick( LABEL_EXPECTED,
      "Label is expected" ,
      "���x�����Ȃ���΂Ȃ�܂���" )
pick( LABEL_NOT_DEFINED,
      "Label is not defined: %s" ,
      "���x���͒�`����Ă��܂���" )
pick( MORE_THAN_ONE_OVERRIDE,
      "More than one override" ,
      "2�ȏ�̃I�[�o�[���C�h������܂�" )
pick( SEGMENT_GROUP_OR_SEGREG_EXPECTED,
      "Segment, group or segment register expected" ,
      "Segment, group or segment register expected" )
pick( UNEXPECTED_END_OF_FILE,
      "Unexpected end of file" ,
      "�r���Ńt�@�C�����I���܂���" )
pick( LABEL_TOO_LONG,
      "Label is too long" ,
      "���x�����������܂�" )
pick( INCONSISTENT_INTERNAL_TABLES,
      "Inconsistent internal tables: '%.*s' not found" ,
      "Inconsistent internal tables: '%.*s' not found" )
pick( CANNOT_OFFSET_GRP,
      "Can not take offset of group" ,
      "�O���[�v�̃I�t�Z�b�g�͎��܂���" )
pick( INVALID_CHARACTER,
      "Invalid character found" ,
      "�g�p�ł��Ȃ�����������܂�" )
pick( INVALID_SIZE,
      "Invalid operand size for instruction" ,
      "���߂ɑ΂��ăI�y�����h�̑傫�����s�K�؂ł�" )
pick( NOT_SUPPORTED,
      "Not supported: %s" ,
      "Not supported: %s" )
pick( SIZE_NOT_SPECIFIED_ASSUMING,
      "Size not specified, assuming: %s" ,
      "Size not specified, assuming: %s" )
pick( FLOAT_OPERAND,
      "Float is used as operand" ,
      "Float is used as operand" )
pick( ONLY_SHORT_AND_NEAR_DISPLACEMENT_IS_ALLOWED,
      "Only SHORT and NEAR displacement is allowed" ,
      "SHORT,NEAR�f�B�X�v���[�X�����g�̂݉\�ł�" )
pick( REPX_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "REPZ, REPNZ, REPE or REPNE prefix is not allowed for this instruction" ,
      "REPZ, REPNZ, REPE or REPNE prefix is not allowed for this instruction" )
pick( SIZE_TOO_LARGE,
      "Initializer magnitude too large for specified size" ,
      "Initializer magnitude too large for specified size" )
pick( SEGMENT_ATTRIBUTE_DEFINED_ALREADY,
      "Segment attribute is defined already: %s" ,
      "�Z�O�����g�p�����[�^�͊��ɒ�`����Ă��܂�" )
pick( MODEL_PARA_DEFINED,
      "Model parameter is defined already" ,
      "���f���p�����[�^�͊��ɒ�`����Ă��܂�" )
pick( SEGMENT_DEF_ERROR,
      "Syntax error in segment definition" ,
      "�Z�O�����g��`�ō\���G���[���N���Ă��܂�" )
pick( SEGDEF_CHANGED,
      "Segment definition changed: %s, %s" ,
      "�Z�O�����g��`���ς�܂���" )
pick( LNAME_TOO_LONG,
      "Lname is too long" ,
      "Lname���������܂�" )
pick( BLOCK_NESTING_ERROR,
      "Block nesting error: %s" ,
      "�u���b�N�E�l�X�g�E�G���[" )
pick( SEGMENT_NOT_OPENED,
      "Ends a segment which is not opened" ,
      "�I�[�v������Ă��Ȃ��Z�O�����g��ends������܂�" )
pick( UNKNOWN_SEGMENT_ATTRIBUTE,
      "Segment attribute is unknown: %s" ,
      "Segment attribute is unknown: %s" )
pick( UNKNOWN_MODEL_OPTION,
      "Unknown .model option: %s" ,
      "Unknown .model option: %s" )
pick( MUST_BE_IN_SEGMENT_BLOCK,
      "Must be in segment block" ,
      "���݃I�[�v������Ă���Z�O�����g�͂���܂���" )
pick( LNAME_USED_ALREADY,
      "Lname is used already" ,
      "Lname�͊��Ɏg�p����Ă��܂�" )
pick( SEG_NOT_DEFINED,
      "Segment %s is not defined" ,
      "Segment %s is not defined" )
pick( COLON_EXPECTED,
      "Colon is expected" ,
      "�R����(:)���K�v�ł�" )
pick( TOKEN_EXPECTED_AFTER_COLON,
      "A token is expected after colon" ,
      "�R����(:)�̌�ɂ̓g�[�N�����K�v�ł�" )
pick( INVALID_QUALIFIED_TYPE,
      "Invalid qualified type" ,
      "�����ȏC���^�C�v�ł�" )
pick( QUALIFIED_TYPE_EXPECTED,
      "Qualified type is expected" ,
      "�C���^�C�v���K�v�ł�" )
pick( EXT_DEF_DIFF,
      "External definition different from previous one" ,
      "�O����`���O�̒�`�ƈقȂ�܂�" )
pick( NO_MEMORY_MODEL_FOUND,
      "Memory model is not found in .MODEL" ,
      ".MODEL�̒��Ƀ��������f��������܂���" )
pick( CANNOT_OPEN_INCLUDE_FILE,
      "Cannot open include file '%s'" ,
      "�C���N���[�h�E�t�@�C�����I�[�v���ł��܂��� %s" )
pick( LIBRARY_NAME_MISSING,
      "Library name is missing" ,
      "���C�u������������܂���" )
pick( SEG_NAME_MISSING,
      "Segment name is missing" ,
      "�Z�O�����g��������܂���" )
pick( GRP_NAME_MISSING,
      "Group name is missing" ,
      "�O���[�v��������܂���" )
pick( DATA_EMITTED_WITH_NO_SEGMENT,
      "Data emitted with no segment" ,
      "�f�[�^���Z�O�����g�ɑ����Ă��܂���" )
pick( SEGLOCATION_EXPECTED,
      "Seglocation is expected" ,
      "�Z�O�����g�E���P�[�V�������K�v�ł�" )
pick( INVALID_REGISTER,
      "This register isn't supported by ASSUME" ,
      "�����ȃ��W�X�^�ł�" )
pick( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS,
      "Cannot access label with segment registers: %s" ,
      "ASSUME���ꂽ���W�X�^�ŃA�h���X���邱�Ƃ͂ł��܂���" )
pick( INVALID_START_ADDRESS,
      "Invalid start address" ,
      "�����ȃX�^�[�g�A�h���X�ł�" )
pick( TOKEN_TOO_LONG,
      "Token is too long" ,
      "�g�[�N�����������܂�" )
pick( EXPANDED_LINE_TOO_LONG,
      "Line too long after expansion: %40s" ,
      "�G�N�X�p���W�����̌�̍s���������܂�" )
pick( LABEL_EXPECTED_AFTER_COLON,
      "A label is expected after colon" ,
      "�R����(:)�̌�ɂ̓��x�����K�v�ł�" )
pick( MUST_BE_ASSOCIATED_WITH_CODE,
      "Must be associated with code" ,
      "�R�[�h�Ɗ֌W���Ă��Ȃ���΂Ȃ�܂���" )
pick( PROC_MUST_HAVE_A_NAME,
      "Procedure must have a name" ,
      "�v���V�[�W���ɂ͖��O���Ȃ���΂Ȃ�܂���" )
pick( LANG_MUST_BE_SPECIFIED,
      "Language type must be specified" ,
      "����^�C�v����`����Ă��Ȃ���΂Ȃ�܂���" )
pick( LOCAL_VAR_MUST_FOLLOW_PROC,
      "Local variable must immediately follow PROC or MACRO statement" ,
      "���[�J���ϐ���PROC���̒���ɂȂ���΂Ȃ�܂���" )
pick( CANNOT_NEST_PROCEDURES,
      "Cannot nest procedures" ,
      "�v���V�[�W�����l�X�g���邱�Ƃ͂ł��܂���" )
pick( NO_PROC_IS_CURRENTLY_DEFINED,
      "No procedure is currently defined" ,
      "�v���V�[�W������`����Ă��܂���" )
pick( VARARG_REQUIRES_C_CALLING_CONVENTION,
      "Vararg requires C calling convention" ,
      "vararg�ɂ�C�̌ďo���K�񂪕K�v�ł�" )
pick( MODEL_DECLARED_ALREADY,
      "Model declared already" ,
      "���ɐ錾����Ă��郂�f���ł�" )
pick( MODEL_IS_NOT_DECLARED,
      "Model is not declared" ,
      "���f���͐錾����Ă��܂�" )
pick( BACKQUOTE_EXPECTED,
      "Backquote expected" ,
      "�t���p��(`)���K�v�ł�" )
pick( COMMENT_DELIMITER_EXPECTED,
      "COMMENT delimiter expected" ,
      "COMMENT��؂蕶�����K�v�ł�" )
pick( END_DIRECTIVE_REQUIRED,
      "END directive required at end of file" ,
      "�t�@�C���̏I�[�ɂ́AEND�^�����߂��K�v�ł�" )
pick( NESTING_LEVEL_TOO_DEEP,
      "Nesting level too deep" ,
      "�l�X�g�E���x�����[�����܂�" )
pick( MACRO_NESTING_LEVEL_TOO_DEEP,
      "Macro nesting level too deep" ,
      "Macro nesting level too deep" )
pick( SYMBOL_NOT_DEFINED,
      "Symbol not defined : %s" ,
      "Symbol not defined : %s" )
pick( SPACES_NOT_ALLOWED_IN_COMMAND_LINE_OPTIONS,
      "Spaces not allowed in command line options" ,
      "�R�}���h���C���I�v�V�����ŋ󔒂͎g�p�ł��܂���" )
pick( MSG_FATAL_ERROR,
      "Fatal error" ,
      "�G���[" )
pick( SOURCE_FILE,
      "Source File" ,
      "�\�[�X�t�@�C��" )
pick( NO_FILENAME_SPECIFIED,
      "No filename specified." ,
      "�t�@�C�������w�肳��Ă��܂���" )
pick( OUT_OF_MEMORY,
      "Out of Memory" ,
      "�������s���ł�" )
pick( CANNOT_OPEN_FILE,
      "Cannot open file: %s" ,
      "Cannot open file: %s" )
pick( CANNOT_CLOSE_FILE,
      "Cannot close file: %s" ,
      "Cannot close file: %s" )
pick( FILE_WRITE_ERROR,
      "File write error: %s",
      "File write error: %s" )
pick( FILE_LSEEK_ERROR,
      "File lseek error: %s" ,
      "File lseek error: %s" )
pick( MSG_UNKNOWN_OPTION,
      "Unknown option %s. Use /? for list of options." ,
      "%s �͖����ȃI�v�V�����ł��B/?�ŃI�v�V�����̃��X�g��\�����܂�\n" )
pick( MSG_INTERNAL_ERROR,
      "Internal error in %s(%u)" ,
      "%s(%u) �œ����G���[���N����܂���\n" )
pick( PARM_REQUIRED,
      "Parameter Required" ,
      "�p�����[�^���K�v�ł�" )
pick( EXPECTED_CL_SQ_BRACKET,
      "Expecting closing square bracket" ,
      "�E�����ʂ��K�v�ł�" )
pick( EXPECTED_FILE_NAME,
      "Expecting file name" ,
      "�t�@�C�������K�v�ł�" )
pick( NO_FP_WITH_FPC_SET,
      "Floating point instruction not allowed with /fpc" ,
      "/fpc�w�莞�ɂ́A���������_���߂͎g�p�ł��܂���" )
pick( TOO_MANY_ERRORS,
      "Too many errors" ,
      "�G���[���������܂�" )
#if INVOKE_WC
pick( TBYTE_NOT_SUPPORTED,
      "Ten byte variables not supported in register calling convention" ,
      "10�o�C�g�ϐ��̓��W�X�^�ďo�K��ł́A�T�|�[�g����Ă��܂���" )
pick( STRANGE_PARM_TYPE,
      "Parameter type not recognised" ,
      "�p�����[�^�^������܂���" )
#endif
pick( FORCED,
      "forced error" ,
      "forced error")
pick( FORCED_ARBITRARY,
      "forced error: %s" ,
      "�����G���[" )
pick( FORCED_NOT_ZERO,
      "forced error: Value not equal to 0 : %d " ,
      "�����G���[: �l��0�ł͂���܂��� : %d " )
pick( FORCED_EQUAL,
      "forced error: Value equal to 0: %d " ,
      "�����G���[: �l��0�ł�: %d " )
pick( FORCED_DEF,
      "forced error: symbol defined: %s" ,
      "�����G���[: �V���{������`����Ă��܂�: %s " )
pick( FORCED_NOT_DEF,
      "forced error: symbol not defined: %s" ,
      "�����G���[: �V���{������`����Ă��܂���: %s " )
pick( FORCED_BLANK,
      "forced error: string blank : <%s>" ,
      "�����G���[: �����񂪋󔒂ł�: <%s>" )
pick( FORCED_NOT_BLANK,
      "forced error: string not blank : <%s>" ,
      "�����G���[: �����񂪋󔒂ł͂���܂���: <%s>" )
pick( FORCED_DIF,
      "forced error: strings not equal : <%s> : <%s>" ,
      "�����G���[: �����񂪓���������܂���: <%s> : <%s>" )
pick( FORCED_IDN,
      "forced error: strings equal : <%s> : <%s> " ,
      "�����G���[: �����񂪓�����: <%s> : <%s>" )
pick( NOTE_INCLUDED_BY,
      "%*s%s(%u): Included by" ,
      "%*s%s(%u): Included by" )
pick( NOTE_MACRO_CALLED_FROM,
      "%*s%s(%u)[%s]: Macro called from" ,
      "%*s%s(%u)[%s]: Macro called from" )
pick( NOTE_MAIN_LINE_CODE,
      "%*s%s(%u): Main line code" ,
      "%*s%s(%u): Main line code" )
pick( EXTENDING_JUMP,
      "Extending jump" ,
      "�͈͊O�ւ̃W�����v�ł�" ) /* fixme - jump out of range */
pick( DIRECTIVE_IGNORED,
      "Directive ignored: %s" ,
      "Directive ignored: %s" )
pick( UNKNOWN_MANGLER,
      "Unknown symbol class '%s'" ,
      "���m�̃V���{���N���X '%s'" )
pick( CONFLICTING_MANGLER,
      "Symbol class for '%s' already established" ,
      "'%s' �̃V���{���N���X�͊��Ɋm�肵�Ă��܂�" )
pick( POWER_OF_2,
      "number must be a power of 2",
      "���l��2�ׂ̂���łȂ���΂Ȃ�܂���" )
pick( ALIGN_TOO_HIGH,
      "alignment request greater than segment alignment",
      "����(�ײ����)�̎w�肪�Z�O�����g������傫���Ȃ��Ă��܂�" )
pick( SYMBOL_PREVIOUSLY_DEFINED,
      "'%s' is already defined",
      "'%s' �͊��ɒ�`����Ă��܂�" )
pick( WRONG_CPU_FOR_32BIT_SEGMENT,
      "Wrong CPU type for 32-bit segment" ,
      "Wrong CPU type for 32-bit segment")
pick( PROC_IS_NOT_CLOSED,
      "Procedure %s is not closed" ,
      "Procedure %s is not closed")
pick( CALL_FAR_TO_NEAR,
      "Far call is converted to near call." ,
      "Far call is converted to near call." )
pick( MSG_CPU_OPTION_INVALID,
      "CPU option %s is not valid for selected CPU.\n" ,
      "CPU option %s is not valid for selected CPU.\n" )
pick( SEGMENT_IN_ANOTHER_GROUP,
      "Segment '%s' is in another group already" ,
      "Segment '%s' is in another group already" )
pick( LABEL_OUTSIDE_SEGMENT,
      "Label is defined outside segment" ,
      "Label is defined outside segment" )
pick( SIZEOF_NEEDS_TYPE_OR_DATA_LABEL,
      "SIZEOF needs type or data label as argument" ,
      "SIZEOF needs type or data label as argument" )
pick( INVALID_USE_OF_LENGTH_SIZE_OPERATOR,
      "Invalid use of LENGTH/LENGTHOF/SIZE/SIZEOF operator" ,
      "Invalid use of LENGTH/LENGTHOF/SIZE/SIZEOF operator" )
pick( TYPE_MUST_HAVE_A_NAME,
      "Typedef must have a name" ,
      "Typedef must have a name" )
pick( SYMBOL_TYPE_CONFLICT,
      "Symbol type conflict: %s" ,
      "Symbol type conflict: %s" )
pick( CONFLICTING_PARAMETER_DEFINITION,
      "Conflicting parameter definition: %s" ,
      "Conflicting parameter definition: %s" )
pick( PROC_AND_PROTO_CALLING_CONV_CONFLICT,
      "PROC and PROTO calling convention conflict" ,
      "PROC and PROTO calling convention conflict" )
pick( NON_BENIGN_STRUCT_REDEFINITION,
      "Non-benign structure redefinition" ,
      "Non-benign structure redefinition" )
pick( TOO_MANY_BITS_IN_RECORD,
      "Too many bits in record" ,
      "Too many bits in record" )
pick( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION,
      "Statement not allowed inside structure definition" ,
      "Statement not allowed inside structure definition" )
pick( UNMATCHED_BLOCK_NESTING,
      "Unmatched block nesting" ,
      "Unmatched block nesting" )
pick( SYMBOL_REDEFINITION,
      "Symbol redefinition: %s" ,
      "Symbol redefinition: %s" )
pick( TEXT_ITEM_REQUIRED,
      "Text item required" ,
      "Text item required" )
pick( INVOKE_ARGUMENT_TYPE_MISMATCH,
      "INVOKE argument type mismatch: argument %u" ,
      "INVOKE argument type mismatch: argument %u" )
pick( TOO_FEW_ARGUMENTS_TO_INVOKE,
      "Too few arguments to INVOKE: %s" ,
      "Too few arguments to INVOKE: %s" )
pick( VARARG_PARAMETER_MUST_BE_LAST,
      "VARARG parameter must be last" ,
      "VARARG parameter must be last" )
pick( TOO_MANY_ARGUMENTS_IN_MACRO_CALL,
      "Too many arguments in macro call: %s" ,
      "Too many arguments in macro call: %s" )
pick( MISSING_OPERATOR_IN_EXPRESSION,
      "Missing operator in expression" ,
      "Missing operator in expression" )
pick( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION,
      "Unexpected literal found in expression: %s" ,
      "Unexpected literal found in expression: %s" )
pick( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM,
      "Initializer must be a string or single item" ,
      "Initializer must be a string or single item" )
pick( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE,
      "Too many initial values for structure: %s" ,
      "Too many initial values for structure: %s" )
pick( TOO_MANY_INITIAL_VALUES_FOR_ARRAY,
      "Too many initial values for array: %s" ,
      "Too many initial values for array: %s" )
pick( STRING_OR_TEXT_LITERAL_TOO_LONG,
      "String or text literal too long" ,
      "String or text literal too long" )
pick( PROLOGUE_MUST_BE_MACRO_FUNC,
      "PROLOGUE must be macro function" ,
      "PROLOGUE must be macro function" )
pick( EPILOGUE_MUST_BE_MACRO_PROC,
      "EPILOGUE must be macro procedure: %s" ,
      "EPILOGUE must be macro procedure: %s" )
pick( RESERVED_WORD_EXPECTED,
      "Reserved word expected" ,
      "Reserved word expected" )
pick( INVOKE_REQUIRES_PROTOTYPE,
      "INVOKE requires prototype for procedure" ,
      "INVOKE requires prototype for procedure" )
pick( INVALID_TYPE_FOR_DATA_DECLARATION,
      "Invalid type for data declaration: %s" ,
      "Invalid type for data declaration: %s" )
pick( OPERAND_MUST_BE_RECORD,
      "Operand must be RECORD type or field" ,
      "Operand must be RECORD type or field" )
pick( UNMATCHED_MACRO_NESTING,
      "Unmatched macro nesting" ,
      "Unmatched macro nesting" )
pick( EMPTY_STRING,
      "Empty (null) string" ,
      "Empty (null) string" )
pick( SEGMENT_MISSING_FOR_FIXUP,
      "No segment info for '%s' to create fixup" ,
      "No segment info for '%s' to create fixup" )
pick( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE,
      "Register value overwritten by INVOKE" ,
      "Register value overwritten by INVOKE" )
pick( MISSING_QUOTE_IN_STRING,
      "Missing quotation mark in string" ,
      "Missing quotation mark in string" )
pick( DIVIDE_BY_ZERO_IN_EXPR,
      "Divide by zero in expression" ,
      "Divide by zero in expression" )
pick( GENERAL_FAILURE,
      "General Failure" ,
      "General Failure" )
pick( NO_FAR_JUMP_TO_NEAR_LABEL,
      "Cannot have implicit far jump or call to near label",
      "Cannot have implicit far jump or call to near label" )
pick( INVALID_USE_OF_REGISTER,
      "Invalid use of register",
      "Invalid use of register" )
pick( DISTANCE_INVALID,
      "Distance invalid for current segment",
      "Distance invalid for current segment" )
pick( INITIALIZER_MAGNITUDE_TOO_LARGE,
      "Initializer magnitude too large",
      "Initializer magnitude too large" )
pick( CANNOT_ADD_TWO_RELOC_LABELS,
      "Cannot add two relocatable labels",
      "Cannot add two relocatable labels" )
pick( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL,
      "Cannot define as public or external: %s",
      "Cannot define as public or external: %s" )
pick( POSITIVE_VALUE_EXPECTED,
      "Positive value expected",
      "Positive value expected" )
pick( FAR_NOT_ALLOWED_IN_FLAT_MODEL_COMM_VARIABLES,
      "FAR not allowed in FLAT model COMM variables",
      "FAR not allowed in FLAT model COMM variables" )
pick( TOO_MANY_ARGUMENTS_TO_INVOKE,
      "Too many arguments to INVOKE" ,
      "Too many arguments to INVOKE" )
pick( DIRECTIVE_MUST_APPEAR_INSIDE_A_MACRO,
      "Directive must appear inside a macro" ,
      "Directive must appear inside a macro" )
pick( INVALID_TYPE_EXPRESSION,
      "Invalid type expression" ,
      "Invalid type expression" )
pick( CANNOT_DECLARE_SCOPED_CODE_LABEL_AS_PUBLIC,
      "Cannot declare scoped code label as PUBLIC: %s" ,
      "Cannot declare scoped code label as PUBLIC: %s" )
pick( INVALID_RADIX_TAG,
      "Invalid radix tag" ,
      "Invalid radix tag" )
pick( INSTRUCTION_OPERAND_MUST_HAVE_SIZE,
      "Instruction operand must have size" ,
      "Instruction operand must have size" )
pick( USE_OF_REGISTER_ASSUMED_TO_ERROR,
      "Use of register assumed to ERROR" ,
      "Use of register assumed to ERROR" )
pick( INITIALIZED_DATA_NOT_SUPPORTED_IN_BSS_SEGMENT,
      "Initialized data not supported in BSS segment" ,
      "Initialized data not supported in BSS segment" )
pick( LITERAL_EXPECTED,
      "Literal expected after '='" ,
      "Literal expected after '='" )
pick( LANG_CONV_NOT_SUPPORTED,
      "Watcom C call convention not supported by INVOKE" ,
      "Watcom C call convention not supported by INVOKE" )
pick( MSG_MS386_NO_4KPAGE,
      "No 4k Page-aligned segments in MS386 OMF" ,
      "MS386 OMF�ɂ�4K�y�[�W����Z�O�����g������܂���" )
pick( MSG_MS386_NO_ACCESS,
      "Access classes (RW, EO, RO, ER) not supported in MS386 OMF" ,
      "MS386 OMF�ł̓A�N�Z�X�N���X(RW,EO,RO,ER)�̓T�|�[�g����Ă��܂���" )
pick( GROUP_DIRECTIVE_INVALID_FOR_COFF,
      "GROUP directive invalid for COFF and ELF format" ,
      "GROUP directive invalid for COFF and ELF format" )
pick( OPERAND_MUST_BE_RELOCATABLE,
      "Operand must be relocatable" ,
      "Operand must be relocatable" )
pick( IF2_NOT_ALLOWED,
      "[ELSE]IF2/.ERR2 not allowed, single-pass assembler" ,
      "[ELSE]IF2/.ERR2 not allowed, single-pass assembler" )
pick( EXPR_TOO_COMPLEX_FOR_UNTILCXZ,
      "Expression too complex for UNTILCXZ" ,
      "Expression too complex for UNTILCXZ" )
pick( OPERANDS_MUST_BE_IN_SAME_SEGMENT,
      "Operands must be in same segment" ,
      "Operands must be in same segment" )
pick( INVALID_USE_OF_EXTERNAL_SYMBOL,
      "Invalid use of external symbol" ,
      "Invalid use of external symbol" )
pick( LEADING_UNDERSCORE_REQUIRED_FOR_START_LABEL,
      "For /coff leading underscore required for start label: %s" ,
      "For /coff leading underscore required for start label: %s" )
pick( UNKNOWN_FIXUP_TYPE,
      "Unknown fixup type found: %u" ,
      "Unknown fixup type found: %u" )
pick( INVALID_CMDLINE_VALUE,
      "Invalid command-line value, default is used: %s" ,
      "Invalid command-line value, default is used: %s" )
pick( UNSUPPORTED_FIXUP_TYPE,
      "Unsupported fixup type for %s: %s" ,
      "Unsupported fixup type for %s: %s" )
pick( INVALID_FIXUP_TYPE,
      "Fixup invalid for %s: %u" ,
      "Fixup invalid for %s: %u" )
pick( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE,
      "Syntax error in control-flow directive" ,
      "Syntax error in control-flow directive" )
pick( INVALID_MODEL_PARAM_FOR_FLAT,
      "Invalid .model parameter for flat model" ,
      "Invalid .model parameter for flat model" )
#if BIN_SUPPORT
pick( FORMAT_DOESNT_SUPPORT_EXTERNALS,
      "Output format doesn't support externals" ,
      "Output format doesn't support externals" )
pick( SEGMENT_FIXUPS_INVALID,
      "Segment fixups invalid for BIN format: %u" ,
      "Segment fixups invalid for BIN format: %u" )
pick( START_LABEL_INVALID,
      "Invalid start label for /bin",
      "Invalid start label for /bin" )
#endif
pick( INDEX_PAST_END_OF_STRING,
      "Index %u is past end of string",
      "Index %u is past end of string" )
pick( COUNT_VALUE_TOO_LARGE,
      "Count value too large",
      "Count value too large" )
pick( COUNT_MUST_BE_POSITIVE_OR_ZERO,
      "Count must be positive or zero",
      "Count must be positive or zero" )
pick( SYNTAX_ERROR_EX,
      "Syntax error: %s" ,
      "Syntax error: %s" )
pick( DIRECTIVE_IGNORED_FOR_COFF,
      "Directive ignored for COFF and ELF format: %s" ,
      "Directive ignored for COFF and ELF format: %s" )
pick( EXPRESSION_NOT_A_CONSTANT,
      "Value of expression is not a constant" ,
      "Value of expression is not a constant" )
pick( MUST_USE_FLOAT_INITIALIZER,
      "Must use floating-point initializer" ,
      "Must use floating-point initializer" )
pick( ORG_NOT_ALLOWED_IN_UNIONS,
      "ORG directive not allowed in unions" ,
      "ORG directive not allowed in unions" )
pick( STRUCT_ALIGN_TOO_HIGH,
      "Struct alignment must be 1, 2, 4, 8, 16 or 32" ,
      "Struct alignment must be 1, 2, 4, 8, 16 or 32" )
pick( STRUCT_CANNOT_BE_INSTANCED,
      "Structure cannot be instanced" ,
      "Structure cannot be instanced" )
pick( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL,
      "Missing angle bracket or brace in literal" ,
      "Missing angle bracket or brace in literal" )
pick( NONDIGIT_IN_NUMBER,
      "Nondigit in number: %s" ,
      "Nondigit in number: %s" )
pick( WORD_FIXUP_FOR_32BIT_LABEL,
      "16bit fixup for 32bit label: %s" ,
      "16bit fixup for 32bit label: %s" )
pick( TOO_MANY_MACRO_PLACEHOLDERS,
      "Too many macro placeholders" ,
      "Too many macro placeholders" )
pick( REQUIRED_PARAMETER_MISSING,
      "%s - parameter %s is missing" ,
      "%s - parameter %s is missing" )
pick( DOES_NOT_WORK_WITH_32BIT_SEGMENTS,
      "%s does not work with 32-bit segments" ,
      "%s does not work with 32-bit segments" )
pick( SEGMENT_EXCEEDS_64K_LIMIT,
      "Segment exceeds 64k limit: %s" ,
      "Segment exceeds 64k limit: %s" )
pick( NOT_SUPPORTED_WITH_OMF_FORMAT,
      "Not supported with OMF format: %s" ,
      "Not supported with OMF format: %s" )
pick( NOT_SUPPORTED_WITH_CURR_FORMAT,
      "Not supported with current output format: %s" ,
      "Not supported with current output format: %s" )
pick( UNKNOWN_DEFAULT_PROLOGUE_ARGUMENT,
      "Unknown default prologue argument: %s" ,
      "Unknown default prologue argument: %s" )
pick( LOADDS_IGNORED_IN_FLAT_MODEL,
      "LOADDS ignored in flat model" ,
      "LOADDS ignored in flat model" )
pick( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION,
      "Missing right parenthesis in expression" ,
      "Missing right parenthesis in expression" )
pick( INVALID_OPERAND_FOR_OPERATOR,
      "Invalid operand for '%s' operator: %s" ,
      "Invalid operand for '%s' operator: %s" )
pick( STRUCTURE_IMPROPERLY_INITIALIZED,
      "Structure improperly initialized: %s" ,
      "Structure improperly initialized: %s" )
pick( EXPECTED,
      "Expected: %s" ,
      "Expected: %s" )
pick( INVALID_DATA_INITIALIZER,
      "Invalid data initializer" ,
      "Invalid data initializer" )
pick( EXPECTED_DATA_LABEL,
      "Expected data label" ,
      "Expected data label" )
pick( EXPRESSION_MUST_BE_A_CODE_ADDRESS,
      "Expression must be a code address" ,
      "Expression must be a code address" )
pick( N_OPTION_NEEDS_A_NAME_PARAMETER,
      "-n Option needs a valid name parameter" ,
      "-n Option needs a valid name parameter" )
pick( CONSTANT_VALUE_TOO_LARGE,
      "Constant value too large" ,
      "Constant value too large" )
pick( TEXT_MACRO_USED_BEFORE_DEFINITION,
      "Text macro was used before definition: %s" ,
      "Text macro was used before definition: %s" )
pick( OFFSET_SIZE_MISMATCH,
      "Offset size incompatible with current segment" ,
      "Offset size incompatible with current segment" )
pick( INSTRUCTION_FORM_REQUIRES_80386,
      "Instruction form requires 80386" ,
      "Instruction form requires 80386" )
pick( GROUP_SEGMENT_SIZE_CONFLICT,
      "Group/Segment offset size conflict: %s - %s" ,
      "Group/Segment offset size conflict: %s - %s" )
pick( ASSEMBLY_PASSES,
      "Assembly passes reached: %u" ,
      "Assembly passes reached: %u" )
pick( FILENAME_MUST_BE_ENCLOSED_IN_QUOTES_OR_BRACKETS,
      "Filename parameter must be enclosed in <> or quotes" ,
      "Filename parameter must be enclosed in <> or quotes" )