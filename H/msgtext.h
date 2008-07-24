pick( SIZE_DOESNT_MATCH_WITH_PREVIOUS_DEFINITION,
      "Size doesn't match with previous definition" ,
      "前の定義とサイズが一致していません" )
pick( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING,
      "Invalid instruction with current CPU setting" ,
      "現在のCPUの設定に対して無効な命令です" )
pick( LOCK_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "LOCK prefix is not allowed on this instruction" ,
      "この命令に対して LOCK プレフィックスは使用できません" )
pick( REP_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "REP prefix is not allowed on this instruction" ,
      "この命令に対して REP プレフィックスは使用できません" )
pick( INVALID_MEMORY_POINTER,
      "Invalid memory pointer" ,
      "無効なメモリポインタです" )
pick( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 addressing mode with current CPU setting" ,
      "現在のCPUの設定では386アドレスを使用できません" )
pick( TOO_MANY_BASE_REGISTERS,
      "Too many base registers" ,
      "ベースレジスタが多すぎます" )
pick( INVALID_INDEX_REGISTER,
      "Invalid index register" ,
      "インデックスレジタが無効です" )
pick( SCALE_FACTOR_MUST_BE_1_2_4_OR_8,
      "Scale factor must be 1, 2, 4 or 8" ,
      "スケールファクタは1,2,4,8のいずれかでなければなりません" )
pick( INVALID_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING,
      "invalid addressing mode with current CPU setting" ,
      "現在のCPUの設定に対して無効なアドレスモードの指定です" )
pick( ESP_CANNOT_BE_USED_AS_INDEX,
      "ESP cannot be used as index" ,
      "ESP はインデックスとしては使用できません" )
pick( TOO_MANY_BASE_INDEX_REGISTERS,
      "Too many base/index registers" ,
      "ベース／インデックス レジスタが多すぎます" )
pick( MEMORY_OFFSET_CANNOT_REFERENCE_TO_MORE_THAN_ONE_LABEL,
      "Memory offset cannot reference to more than one label" ,
      "メモリオフセットは２個以上のラベルへの参照を行えません" )
pick( OFFSET_MUST_BE_RELOCATABLE,
      "Offset must be relocatable" ,
      "オフセットはリロケｰタブルでなければなりません" )
pick( MEMORY_OFFSET_EXPECTED,
      "Memory offset expected" ,
      "メモリオフセットでなければなりません" )
pick( INVALID_INDIRECT_MEMORY_OPERAND,
      "Invalid indirect memory operand" ,
      "無効な間接メモリオペランドです" )
pick( CANNOT_MIX_16_AND_32_BIT_REGISTERS,
      "Cannot mix 16 and 32-bit registers" ,
      "16ビットレジスタと32ビットレジスタは混在できません" )
pick( CPU_TYPE_ALREADY_SET,
      "CPU type already set" ,
      "CPUのタイプは既に設定されています" )
pick( UNKNOWN_DIRECTIVE,
      "Unknown directive: %s" ,
      "無効なディレクティブです" )
pick( EXPECTING_COMMA,
          "Expecting comma" ,
          "コンマが必要です" )
pick( EXPECTING_NUMBER,
      "Expecting number" ,
      "数字が必要です" )
pick( INVALID_LABEL_DEFINITION,
      "Invalid label definition" ,
      "無効なラベル定義です" )
pick( INVALID_USE_OF_SHORT_NEAR_FAR_OPERATOR,
      "Invalid use of SHORT, NEAR, FAR operator" ,
      "SHORT, NEAR, FARオペレータの使用方法が無効です" )
pick( NO_MEMORY,
      "No memory" ,
      "メモリがありません" )
pick( CANNOT_USE_386_SEGMENT_REGISTER_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 segment register with current CPU setting" ,
      "現在のCPUの設定では386セグメントレジスタは使用できません" )
pick( POP_CS_IS_NOT_ALLOWED,
      "POP CS is not allowed" ,
      "POP CSはできません" )
pick( CANNOT_USE_386_REGISTER_WITH_CURRENT_CPU_SETTING,
      "Cannot use 386 register with current CPU setting" ,
      "現在のCPUの設定では386レジスタは使用できません" )
pick( ONLY_MOV_CAN_USE_SPECIAL_REGISTER,
      "Only MOV can use special register" ,
      "特別レジスタは MOV 命令でのみ使用できます" )
pick( CANNOT_USE_TR3_TR4_TR5_IN_CURRENT_CPU_SETTING,
      "Cannot use TR3, TR4, TR5 in current CPU setting" ,
      "現在のCPUの設定では TR3, TR4, TR5 は使用できません" )
pick( CANNOT_USE_SHORT_WITH_CALL,
      "Cannot use SHORT with CALL" ,
      "CALL命令と共に SHORT を使うことはできません" )
pick( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED,
      "Only SHORT displacement is allowed" ,
      "SHORTディスプレースメントのみ可能です" )
pick( SYNTAX_ERROR,
      "Syntax error" ,
      "構文エラー" )
pick( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION,
      "Prefix must be followed by an instruction" ,
      "プレフィックスの後には命令がなければなりません" )
pick( NO_SIZE_GIVEN_BEFORE_PTR_OPERATOR,
      "No size given before 'PTR' operator" ,
      "PTRオペレータの前にサイズ指定がありません" )
pick( INVALID_IMUL_FORMAT,
      "Invalid IMUL format" ,
      "IMUL命令の使用形式が不適切です" )
pick( INVALID_SHLD_SHRD_FORMAT,
      "Invalid SHLD/SHRD format" ,
      "SHLD/SHRD命令の使用形式が不適切です" )
pick( TOO_MANY_COMMAS,
      "Too many commas" ,
      "コンマ(,)が多すぎます" )
pick( SYNTAX_ERROR_UNEXPECTED_COLON,
      "Syntax error: Unexpected colon" ,
      "構文エラー: 不適切なコロン(:)があります" )
pick( OPERANDS_MUST_BE_THE_SAME_SIZE,
      "Operands must be the same size: %u - %u" ,
      "オペランドは同じサイズでなければなりません" )
pick( INVALID_INSTRUCTION_OPERANDS,
      "Invalid instruction operands" ,
      "無効な命令オペランドです" )
pick( IMMEDIATE_CONSTANT_TOO_LARGE,
      "Immediate constant too large" ,
      "即値定数が大きすぎます" )
pick( CANNOT_USE_SHORT_OR_NEAR,
      "Can not use short or near modifiers with this instruction" ,
      "この命令と一緒にshortまたはnearを使用することはできません" )
pick( JUMP_OUT_OF_RANGE,
      "Jump out of range" ,
      "範囲外へのジャンプです" )
pick( DISPLACEMENT_OUT_OF_RANGE,
      "Displacement cannot be larger than 32k" ,
      "ディスプレースメントが32Kを越えています" )
pick( INITIALIZER_OUT_OF_RANGE,
      "Initializer value too large" ,
      "設定されている初期値が大きすぎます" )
pick( SYMBOL_ALREADY_DEFINED,
      "Symbol '%s' already defined" ,
      "このシンボルは既に定義されています" )
pick( IMMEDIATE_DATA_TOO_BIG,
      "Immediate data too large" ,
      "即値データが大きすぎます" )
pick( IMMEDIATE_DATA_OUT_OF_RANGE,
      "Immediate data out of range" ,
      "即値データの値が範囲外です" )
pick( NO_JUMP_TO_AUTO,
      "Can not transfer control to stack symbol" ,
      "スタックシンボルへ制御を移すことはできません" )
pick( OFFSET_TOO_LARGE,
      "Offset magnitude too large for specified size" ,
      "オフセットはワード(2バイト)より小さくできません" )
pick( CANNOT_OFFSET_AUTO,
      "Can not take offset of stack symbol" ,
      "スタックシンボルのオフセットを取ることはできません" )
pick( CANNOT_SEG_AUTO,
      "Can not take segment of stack symbol" ,
      "スタック・シンボルのセグメントは取れません" )
pick( SEGMENT_TOO_BIG,
      "Segment too large" ,
      "セグメントが大きすぎます" )
pick( OFFSET_TOO_BIG,
      "Offset cannot be larger than 32k" ,
      "オフセットは32Kより大きくできません" )
pick( OP_2_TOO_BIG,
      "Operand 2 too big" ,
      "オペランド2が大きすぎます" )
pick( OP_1_TOO_SMALL,
      "Operand 1 too small" ,
      "オペランド1が小さすぎます" )
pick( TOO_MANY_OPERATOR,
      "Too many arithmetic operators" ,
      "算術計算の演算子が多すぎます" )
pick( TOO_MANY_OPEN_SQUARE_BRACKETS,
      "Too many open square brackets" ,
      "左カギ括弧([)が多すぎます" )
pick( TOO_MANY_CLOSE_SQUARE_BRACKETS,
      "Too many close square brackets" ,
      "右カギ括弧(])が多すぎます" )
pick( TOO_MANY_OPEN_BRACKETS,
      "Too many open brackets" ,
      "左括弧'('が多すぎます" )
pick( TOO_MANY_CLOSE_BRACKETS,
      "Too many close brackets" ,
      "右括弧')'が多すぎます" )
pick( INVALID_NUMBER_DIGIT,
      "Invalid number digit" ,
      "不適切な数値です" )
pick( LINE_TOO_LONG,
      "Line too long" ,
      "アセンブラコードが長すぎます" )
pick( BRACKETS_NOT_BALANCED,
      "Brackets are not balanced" ,
      "左右の括弧の数が合いません" )
pick( OPERATOR_EXPECTED,
      "Operator is expected" ,
      "演算子がなければなりません" )
pick( OPERAND_EXPECTED,
      "Operand is expected" ,
      "オペランドでなければなりません" )
pick( TOO_MANY_TOKENS,
      "Too many tokens in a line" ,
      "1行にあるトークンが多すぎます" )
pick( BRACKET_EXPECTED,
      "Bracket is expected" ,
      "括弧がなければなりません" )
pick( ILLEGAL_USE_OF_REGISTER,
      "Illegal use of register" ,
      "レジスタの使用方法が間違っています" )
pick( ILLEGAL_USE_OF_LABEL,
      "Illegal use of label" ,
      "ラベルの使用方法が間違っています" )
pick( INVALID_OPERAND_IN_ADDITION,
      "Invalid operand in addition" ,
      "加算命令のオペランドが無効です" )
pick( INVALID_OPERAND_IN_SUBTRACTION,
      "Invalid operand in subtraction" ,
      "減算命令のオペランドが無効です" )
pick( ONE_OPERAND_MUST_BE_CONSTANT,
      "One operand must be constant" ,
      "一方のオペランドは定数でなければなりません" )
pick( CONSTANT_EXPECTED,
      "Constant expected" ,
      "Constant expected" )
pick( CONSTANT_OPERAND_EXPECTED,
      "Constant operand is expected" ,
      "定数オペランドでなければなりません" )
pick( ADDITION_CONSTANT_EXPECTED,
      "A constant operand is expected in addition" ,
      "加算命令には定数オペランドがなければなりません" )
pick( SUBTRACTION_CONSTANT_EXPECTED,
      "A constant operand is expected in subtraction" ,
      "減算命令には定数オペランドがなければなりません" )
pick( MULTIPLICATION_CONSTANT_EXPECTED,
      "A constant operand is expected in multiplication" ,
      "乗算命令には定数オペランドがなければなりません" )
pick( DIVISION_CONSTANT_EXPECTED,
      "A constant operand is expected in division" ,
      "除算命令には定数オペランドがなければなりません" )
pick( POSITIVE_SIGN_CONSTANT_EXPECTED,
      "A constant operand is expected after a positive sign" ,
      "正の符号(+)の後には定数オペランドがなければなりません" )
pick( NEGATIVE_SIGN_CONSTANT_EXPECTED,
      "A constant operand is expected after a negative sign" ,
      "負の符号(-)の後には定数オペランドがなければなりません" )
pick( LABEL_NOT_DEFINED,
      "Label %s is not defined" ,
      "ラベルは定義されていません" )
pick( MORE_THAN_ONE_OVERRIDE,
      "More than one override" ,
      "2個以上のオーバーライドがあります" )
pick( LABEL_IS_EXPECTED,
      "Label is expected" ,
      "ラベルがなければなりません" )
pick( ONLY_SEG_OR_GROUP_ALLOWED,
      "Only segment or group label is allowed" ,
      "セグメントあるいはグループのラベルのみが許されます" )
pick( REG_OR_LABEL_EXPECTED_IN_OVERRIDE,
      "Only register or label is expected in override" ,
      "オーバーライドされるのはレジスタまたはラベルのみです" )
pick( UNEXPECTED_END_OF_FILE,
      "Unexpected end of file" ,
      "途中でファイルが終わりました" )
pick( LABEL_TOO_LONG,
      "Label is too long" ,
      "ラベルが長すぎます" )
pick( INTERNAL_ERROR_1,
      "Internal Error #1" ,
      "内部エラー #1" )
pick( CANNOT_OFFSET_GRP,
      "Can not take offset of group" ,
      "グループのオフセットは取れません" )
pick( CANNOT_OFFSET_SEG,
      "Can not take offset of segment" ,
      "セグメントのオフセットは取れません" )
pick( INVALID_CHARACTER,
      "Invalid character found" ,
      "使用できない文字があります" )
pick( INVALID_SIZE,
      "Invalid operand size for instruction" ,
      "命令に対してオペランドの大きさが不適切です" )
pick( NOT_SUPPORTED,
      "Not supported: %s" ,
      "この命令はサポートされていません" )
pick( ASSUMING_BYTE,
      "Size not specified -- BYTE PTR is assumed" ,
      "大きさが指定されていません -- BYTE PTRを仮定します" )
pick( ASSUMING_WORD,
      "Size not specified -- WORD PTR is assumed" ,
      "大きさが指定されていません -- WORD PTRを仮定します" )
pick( ASSUMING_DWORD,
      "Size not specified -- DWORD PTR is assumed" ,
      "大きさが指定されていません -- DWORD PTRを仮定します" )
pick( FLOAT_OPERAND,
      "Float is used as operand" ,
      "Float is used as operand" )
pick( ONLY_SHORT_AND_NEAR_DISPLACEMENT_IS_ALLOWED,
      "Only SHORT and NEAR displacement is allowed" ,
      "SHORT,NEARディスプレースメントのみ可能です" )
pick( MISSING_PTR_OPERATOR,
      "Missing 'PTR' operator" ,
      "Missing 'PTR' operator" )
pick( REPX_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION,
      "REPZ, REPNZ, REPE or REPNE prefix is not allowed on this instruction" ,
      "REPZ, REPNZ, REPE or REPNE prefix is not allowed on this instruction" )
pick( SIZE_TOO_LARGE,
      "Initializer magnitude too large for specified size" ,
      "Initializer magnitude too large for specified size" )
pick( SEGMENT_PARA_DEFINED,
      "Segment parameter is defined already" ,
      "セグメントパラメータは既に定義されています" )
pick( MODEL_PARA_DEFINED,
      "Model parameter is defined already" ,
      "モデルパラメータは既に定義されています" )
pick( SEGMENT_DEF_ERROR,
      "Syntax error in segment definition" ,
      "セグメント定義で構文エラーが起きています" )
pick( SEGDEF_AT_NOT_SUPPORTED,
      "'AT' is not supported in segment definition" ,
      "セグメント定義には、まだ AT はサポートされていません" )
pick( SEGDEF_CHANGED,
      "Segment definition changed: %s, %s" ,
      "セグメント定義が変りました" )
pick( LNAME_TOO_LONG,
      "Lname is too long" ,
      "Lnameが長すぎます" )
pick( BLOCK_NESTING_ERROR,
      "Block nesting error: %s" ,
      "ブロック・ネスト・エラー" )
pick( SEGMENT_NOT_OPENED,
      "Ends a segment which is not opened" ,
      "オープンされていないセグメントのendsがあります" )
pick( UNDEFINED_SEGMENT_OPTION,
      "Segment option is undefined" ,
      "セグメント・オプションが未定義です" )
pick( UNDEFINED_MODEL_OPTION,
      "Model option is undefined" ,
      "モデル・オプションが未定義です" )
pick( NO_SEGMENT_OPENED,
      "No segment is currently opened" ,
      "現在オープンされているセグメントはありません" )
pick( LNAME_USED_ALREADY,
      "Lname is used already" ,
      "Lnameは既に使用されています" )
pick( SEG_NOT_DEFINED,
      "Segment %s is not defined" ,
      "Segment %s is not defined" )
pick( PUBLIC_NOT_DEFINED,
      "Public is not defined" ,
      "パブリックが未定義です" )
pick( COLON_EXPECTED,
      "Colon is expected" ,
      "コロン(:)が必要です" )
pick( TOKEN_EXPECTED_AFTER_COLON,
      "A token is expected after colon" ,
      "コロン(:)の後にはトークンが必要です" )
pick( INVALID_QUALIFIED_TYPE,
      "Invalid qualified type" ,
      "無効な修飾タイプです" )
pick( QUALIFIED_TYPE_EXPECTED,
      "Qualified type is expected" ,
      "修飾タイプが必要です" )
pick( EXT_DEF_DIFF,
      "External definition different from previous one" ,
      "外部定義が前の定義と異なります" )
pick( NO_MEMORY_MODEL_FOUND,
      "Memory model is not found in .MODEL" ,
      ".MODELの中にメモリモデルがありません" )
pick( CANNOT_OPEN_INCLUDE_FILE,
      "Cannot open include file %s" ,
      "インクルード・ファイルをオープンできません %s" )
pick( NAME_USED_ALREADY,
      "Name is used already" ,
      "名前は既に使用されています" )
pick( LIBRARY_NAME_MISSING,
      "Library name is missing" ,
      "ライブラリ名がありません" )
pick( SEG_NAME_MISSING,
      "Segment name is missing" ,
      "セグメント名がありません" )
pick( GRP_NAME_MISSING,
      "Group name is missing" ,
      "グループ名がありません" )
pick( DATA_EMITTED_WITH_NO_SEGMENT,
      "Data emitted with no segment" ,
      "データがセグメントに属していません" )
pick( SEGLOCATION_EXPECTED,
      "Seglocation is expected" ,
      "セグメント・ロケーションが必要です" )
pick( INVALID_REGISTER,
      "This register isn't supported by ASSUME" ,
      "無効なレジスタです" )
pick( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS,
      "Cannot access label with segment registers: %s" ,
      "ASSUMEされたレジスタでアドレスすることはできません" )
pick( INVALID_START_ADDRESS,
      "Invalid start address" ,
      "無効なスタートアドレスです" )
pick( TOKEN_TOO_LONG,
      "Token is too long" ,
      "トークンが長すぎます" )
pick( EXPANDED_LINE_TOO_LONG,
      "The line is too long after expansion" ,
      "エクスパンジョンの後の行が長すぎます" )
pick( LABEL_EXPECTED_AFTER_COLON,
      "A label is expected after colon" ,
      "コロン(:)の後にはラベルが必要です" )
pick( MUST_BE_ASSOCIATED_WITH_CODE,
      "Must be associated with code" ,
      "コードと関係していなければなりません" )
pick( PROC_MUST_HAVE_A_NAME,
      "Procedure must have a name" ,
      "プロシージャには名前がなければなりません" )
pick( LANG_MUST_BE_SPECIFIED,
      "Language type must be specified" ,
      "言語タイプが定義されていなければなりません" )
pick( END_OF_PROCEDURE_NOT_FOUND,
      "End of procedure is not found" ,
      "プロシージャの終わりが見つかりません" )
pick( LOCAL_VAR_MUST_FOLLOW_PROC,
      "Local variable must immediately follow PROC or MACRO statement" ,
      "ローカル変数はPROC文の直後になければなりません" )
pick( EXTRA_CHARACTER_FOUND,
      "Extra character found" ,
      "余分な文字があります" )
pick( CANNOT_NEST_PROCEDURES,
      "Cannot nest procedures" ,
      "プロシージャをネストすることはできません" )
pick( NO_PROC_IS_CURRENTLY_DEFINED,
      "No procedure is currently defined" ,
      "プロシージャが定義されていません" )
pick( PROC_NAME_DOES_NOT_MATCH,
      "Procedure name does not match" ,
      "プロシージャ名が一致しません" )
pick( VARARG_REQUIRES_C_CALLING_CONVENTION,
      "Vararg requires C calling convention" ,
      "varargにはCの呼出し規約が必要です" )
pick( MODEL_DECLARED_ALREADY,
      "Model declared already" ,
      "既に宣言されているモデルです" )
pick( MODEL_IS_NOT_DECLARED,
      "Model is not declared" ,
      "モデルは宣言されています" )
pick( BACKQUOTE_EXPECTED,
      "Backquote expected" ,
      "逆引用符(`)が必要です" )
pick( COMMENT_DELIMITER_EXPECTED,
      "COMMENT delimiter expected" ,
      "COMMENT区切り文字が必要です" )
pick( END_DIRECTIVE_REQUIRED,
      "End directive required at end of file" ,
      "ファイルの終端には、END疑似命令が必要です" )
pick( NESTING_LEVEL_TOO_DEEP,
      "Nesting level too deep" ,
      "ネスト・レベルが深すぎます" )
pick( SYMBOL_NOT_DEFINED,
      "Symbol not defined : %s" ,
      "Symbol not defined : %s" )
pick( INSERT_STUPID_WARNING_1_HERE,
      "Insert Stupid warning #1 here" ,
      "無意味な警告#1をここに挿入します" )
pick( INSERT_STUPID_WARNING_2_HERE,
      "Insert Stupid warning #2 here" ,
      "無意味な警告#2をここに挿入します" )
pick( SPACES_NOT_ALLOWED_IN_COMMAND_LINE_OPTIONS,
      "Spaces not allowed in command line options" ,
      "コマンドラインオプションで空白は使用できません" )
pick( MSG_ERROR,
      "Error: " ,
      "エラー: " )
pick( SOURCE_FILE,
      "Source File" ,
      "ソースファイル" )
pick( NO_FILENAME_SPECIFIED,
      "No filename specified." ,
      "ファイル名が指定されていません" )
pick( OUT_OF_MEMORY,
      "Out of Memory" ,
      "メモリ不足です" )
pick( CANNOT_OPEN_FILE,
      "Cannot Open File - " ,
      "ファイルをオープンできません - " )
pick( CANNOT_CLOSE_FILE,
      "Cannot Close File - " ,
      "ファイルをクローズできません - " )
pick( CANNOT_GET_FILE,
      "Cannot Get Start of Source File - " ,
      "ソースファイルのスタートが得られません - " )
pick( CANNOT_SET_FILE,
      "Cannot Set to Start of Source File - " ,
      "ソースファイルのスタートに設定できません - " )
pick( MSG_INCLUDE_PATH,
      "include path %s." ,
      "パス %s をインクルードします\n" )
pick( MSG_UNKNOWN_OPTION,
      "Unknown option %s. Use /? for list of options." ,
      "%s は無効なオプションです。/?でオプションのリストを表示します\n" )
pick( MSG_READ_MORE,
      "read more command line from %s." ,
      "%s から追加のコマンドラインを読みます\n" )
pick( MSG_INTERNAL_ERROR,
      "Internal error in %s(%u)" ,
      "%s(%u) で内部エラーが起こりました\n" )
pick( OBJECT_WRITE_ERROR,
      "OBJECT WRITE ERROR !!" ,
      "オブジェクト書込みエラー !!\n" )
pick( PARM_REQUIRED,
      "Parameter Required" ,
      "パラメータが必要です" )
pick( EXPECTED_CL_SQ_BRACKET,
      "Expecting closing square bracket" ,
      "右鍵括弧が必要です" )
pick( EXPECTED_FILE_NAME,
      "Expecting file name" ,
      "ファイル名が必要です" )
pick( NO_FP_WITH_FPC_SET,
      "Floating point instruction not allowed with /fpc" ,
      "/fpc指定時には、浮動小数点命令は使用できません" )
pick( ERR_TOO_MANY_ERRORS,
      "Too many errors" ,
      "エラーが多すぎます" )
pick( BT_NOT_RECOG,
      "Build target not recognised" ,
      "ターゲットOSが判りません" )
pick( PUBLIC_CONSTANT_NOT_NUMERIC,
      "Public constants should be numeric; 0 written" ,
      "パブリックな定数は数値でなければなりません; 0を書き込みます" )
pick( SYMBOL_EXPECTED,
      "Expecting symbol" ,
      "シンボルが必要です" )
pick( DONT_MIX_SIM_WITH_REAL_SEGDEFS,
      "Do not mix simplified and full segment definitions" ,
      "簡略(simplified)セグメントと完全(full)セグメントとは、いっしょに使用できません" )
pick( CANT_ACCESS_MULTI_REG_PARMS,
      "Parms passed in multiple registers must be accessed separately, use %s " ,
      "複数のレジスタを使うparmは、別々にアクセスされなければなりません, %s を使用してください" )
pick( TBYTE_NOT_SUPPORTED,
      "Ten byte variables not supported in register calling convention" ,
      "10バイト変数はレジスタ呼出規約では、サポートされていません" )
pick( STRANGE_PARM_TYPE,
      "Parameter type not recognised" ,
      "パラメータ型が判りません" )
pick( FORCED,
      "forced error" ,
      "forced error")
pick( FORCED_ARBITRARY,
      "forced error: %s" ,
      "強制エラー" )
pick( FORCED_NOT_ZERO,
      "forced error: Value not equal to 0 : %d " ,
      "強制エラー: 値が0ではありません : %d " )
pick( FORCED_EQUAL,
      "forced error: Value equal to 0: %d " ,
      "強制エラー: 値が0です: %d " )
pick( FORCED_DEF,
      "forced error: symbol defined: %s" ,
      "強制エラー: シンボルが定義されています: %s " )
pick( FORCED_NOT_DEF,
      "forced error: symbol not defined: %s" ,
      "強制エラー: シンボルが定義されていません: %s " )
pick( FORCED_BLANK,
      "forced error: string blank : <%s>" ,
      "強制エラー: 文字列が空白です: <%s>" )
pick( FORCED_NOT_BLANK,
      "forced error: string not blank : <%s>" ,
      "強制エラー: 文字列が空白ではありません: <%s>" )
pick( FORCED_DIF,
      "forced error: strings not equal : <%s> : <%s>" ,
      "強制エラー: 文字列が等しくありません: <%s> : <%s>" )
pick( FORCED_IDN,
      "forced error: strings equal : <%s> : <%s> " ,
      "強制エラー: 文字列が等しい: <%s> : <%s>" )
pick( NOTE_INCLUDED_BY,
      "included by file %s(%lu)" ,
      "ファイル %s(%lu) に含まれます" )
pick( NOTE_MACRO_CALLED_FROM,
      "macro called from file %s(%lu)" ,
      "ファイル %s(%lu) から呼ばれるマクロです" )
pick( SYMBOL_S_NOT_DEFINED,
      "Symbol %s is not defined" ,
      "シンボルが定義されていません ( %s )" ) /* fixme */
pick( EXTENDING_JUMP,
      "Extending jump" ,
      "範囲外へのジャンプです" ) /* fixme - jump out of range */
pick( IGNORING_DIRECTIVE,
      "Directive '%s' ignored" ,
      "不適切な擬似命令を無視します" )
pick( UNKNOWN_MANGLER,
      "Unknown symbol class '%s'" ,
      "未知のシンボルクラス '%s'" )
pick( CONFLICTING_MANGLER,
      "Symbol class for '%s' already established" ,
      "'%s' のシンボルクラスは既に確定しています" )
pick( POWER_OF_2,
      "number must be a power of 2",
      "数値は2のべき乗でなければなりません" )
pick( ALIGN_TOO_HIGH,
      "alignment request greater than segment alignment",
      "整列(ｱﾗｲﾝﾒﾝﾄ)の指定がセグメント整列より大きくなっています" )
pick( SYMBOL_PREVIOUSLY_DEFINED,
      "'%s' is already defined",
      "'%s' は既に定義されています" )
pick( UNCLOSED_CONDITIONALS,
      "%u unclosed conditional directive(s) detected" ,
      "%u 閉じていない条件擬似命令を検出しました" )
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
pick( SEGMENT_IN_GROUP,
      "Segment is in another group already" ,
      "Segment is in another group already" )
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
      "Conflicting parameter definition" ,
      "Conflicting parameter definition" )
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
      "INVOKE argument type mismatch, parameter %u" ,
      "INVOKE argument type mismatch, parameter %u" )
pick( TOO_FEW_ARGUMENTS_TO_INVOKE,
      "Too few arguments to invoke" ,
      "Too few arguments to invoke" )
pick( VARARG_PARAMETER_MUST_BE_LAST,
      "VARARG parameter must be last" ,
      "VARARG parameter must be last" )
pick( TOO_MANY_ARGUMENTS_IN_MACRO_CALL,
      "Too many arguments in macro call" ,
      "Too many arguments in macro call" )
pick( MISSING_OPERATOR_IN_EXPRESSION,
      "Missing operator in expression" ,
      "Missing operator in expression" )
pick( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION,
      "Unexpected literal found in expression" ,
      "Unexpected literal found in expression" )
pick( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM,
      "Initializer must be a string or single item" ,
      "Initializer must be a string or single item" )
pick( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE,
      "Too many initial values for structure" ,
      "Too many initial values for structure" )
pick( STRING_OR_TEXT_LITERAL_TOO_LONG,
      "String or text literal too long" ,
      "String or text literal too long" )
pick( PROLOGUE_MUST_BE_MACRO_FUNC,
      "PROLOGUE must be macro function" ,
      "PROLOGUE must be macro function" )
pick( EPILOGUE_MUST_BE_MACRO_PROC,
      "EPILOGUE must be macro procedure" ,
      "EPILOGUE must be macro procedure" )
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
pick( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE,
      "Instruction or register not accepted in current CPU mode" ,
      "Instruction or register not accepted in current CPU mode" )
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
      "MS386 OMFには4Kページ整列セグメントがありません" )
pick( MSG_MS386_NO_ACCESS,
      "Access classes (RW, EO, RO, ER) not supported in MS386 OMF" ,
      "MS386 OMFではアクセスクラス(RW,EO,RO,ER)はサポートされていません" )
pick( GROUP_DIRECTIVE_INVALID_FOR_COFF,
      "GROUP directive allowed for OMF format only" ,
      "GROUP directive allowed for OMF format only" )
pick( OPERAND_MUST_BE_RELOCATABLE,
      "Operand must be relocatable" ,
      "Operand must be relocatable" )
pick( IF2_NOT_ALLOWED,
      "[ELSE]IF2/.ERR2 not allowed, single-pass assembler" ,
      "[ELSE]IF2/.ERR2 not allowed, single-pass assembler" )
pick( EXPR_TOO_COMPLEX_FOR_UNTILCXZ,
      "expression too complex for UNTILCXZ" ,
      "expression too complex for UNTILCXZ" )
pick( OPERANDS_MUST_BE_IN_SAME_SEGMENT,
      "operands must be in same segment" ,
      "operands must be in same segment" )
pick( INVALID_USE_OF_EXTERNAL_SYMBOL,
      "invalid use of external symbol" ,
      "invalid use of external symbol" )
pick( LEADING_UNDERSCORE_REQUIRED_FOR_START_LABEL,
      "For /coff leading underscore required for start label: %s" ,
      "For /coff leading underscore required for start label: %s" )
pick( UNKNOWN_FIXUP_TYPE,
      "Unknown fixup type found: %u" ,
      "Unknown fixup type found: %u" )
pick( INVALID_CMDLINE_VALUE,
      "Invalid command-line value, default is used: %s" ,
      "Invalid command-line value, default is used: %s" )
pick( INVALID_ELF_FIXUP_TYPE,
      "Fixup invalid for ELF format: %u" ,
      "Fixup invalid for ELF format: %u" )
pick( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE,
      "Syntax error in control-flow directive" ,
      "Syntax error in control-flow directive" )
pick( INVALID_MODEL_PARAM_FOR_FLAT,
      "Invalid .model parameter for flat model" ,
      "Invalid .model parameter for flat model" )
