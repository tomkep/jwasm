:segment ENGLISH
Usage:   jwasm [options] asm_file [options] [@env_var]
:segment qnx|linux
Options:
:elsesegment
Options:                    ( /option is also supported )
:endsegment
-{0,1,2,3,4,5,6}{p} instructions accepted for 8086(=0), 80186(=1),
                    80286(=2), 80386(=3), 80486(=4), Pentium(=5) or
                    Pentium Pro(=6) cpu. {p} suffix includes privileged
                    instructions.
-bt=<os>            set the build target to <os>
-Cp                 preserve case of identifiers
-D<name>[=text]     define text macro
-e<number>          set error limit number
-EP                 Output preprocessed listing to stdout
-fi=<file_name>     force <file_name> to be included
-Fl=<file_name>     write listing file
-Fo=<file_name>     set object file name
-fr=<file_name>     set error file name
-FPc                calls to floating-point library
-FPi                inline 80x87 instructions with emulation (default)
-FPi87              inline 80x87 instructions
-fp{n}              floating-point instructions for 8087(n=0), 80287(n=2),
                    80387(n=3), Pentium(n=5) or Pentium Pro(n=6).
-I=<directory>      add directory to list of include directories
-j or -s            force signed types to be used for signed values
-m{t,s,m,c,l,h,f}   set memory model:
                    (Tiny, Small, Medium, Compact, Large, Huge, Flat)
:segment HIDDEN
-nc=<name>          set code class name
:endsegment
-nd=<name>          set data segment name
-nm=<name>          set module name
-nt=<name>          set name of text segment
-o                  allow C form of octal constants
-zcm                set C name mangler to MASM compatible mode (default)
-zcw                set C name mangler to Watcom compatible mode
-Zd                 line number debugging support
-zlc                don't create OMF COMMENT records about data in code
-zld                don't create OMF COMMENT records about file dependencies
-zq or -q           don't display version and copyright information
-zz                 remove '@size' from STDCALL function names
-zzo                don't mangle STDCALL symbols (backward compatible)
-? or -h            print this message
-w<number>          set warning level number
-we                 treat all warnings as errors
-wx                 set warning level to the highest level

:elsesegment JAPANESE
使用方法: jwasm [options] file [options] [@env_var]
:segment qnx
オプション:
:elsesegment
オプション:                 ( /ｵﾌﾟｼｮﾝ でも指定できます )
:endsegment
        -fo=<file_name>     オブジェクトファイルを設定します
        -fe=<file_name>     エラーファイル名を指定します
        -i=<directory>      インクルード・ディレクトリのリストを追加します
        -e                  ENDディレクティブでASMファイルの読み込みを止めます
        -j or -s            符号付き型を符号付き値のために使用するようにします
        -d1                 行番号デバッグ情報を出力します
        -d<name>[=text]     テキストマクロを定義します
        -0                  8086 命令
        -1                  80186 命令
        -2                  80286 命令
        -3                  80386 命令
        -4                  80486 命令
        -m{t,s,m,c,l,h,f}   メモリ・モデル
                            (Tiny, Small, Medium, Compact, Large, Huge, Flat)
        -o                  allow C form of octal constants
        -3r                 386 レジスタ呼び出し規約
        -3s                 386 スタック呼び出し規約
        -4r                 486 レジスタ呼び出し規約
        -4s                 486 スタック呼び出し規約
        -zq or -q           メッセージ等の出力をしません
        -?                  このメッセージを表示します
:endsegment
:segment IS_RC
.
:endsegment
