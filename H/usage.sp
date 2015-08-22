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
�g�p���@: jwasm [options] file [options] [@env_var]
:segment qnx
�I�v�V����:
:elsesegment
�I�v�V����:                 ( /��߼�� �ł��w��ł��܂� )
:endsegment
        -fo=<file_name>     �I�u�W�F�N�g�t�@�C����ݒ肵�܂�
        -fe=<file_name>     �G���[�t�@�C�������w�肵�܂�
        -i=<directory>      �C���N���[�h�E�f�B���N�g���̃��X�g��ǉ����܂�
        -e                  END�f�B���N�e�B�u��ASM�t�@�C���̓ǂݍ��݂��~�߂܂�
        -j or -s            �����t���^�𕄍��t���l�̂��߂Ɏg�p����悤�ɂ��܂�
        -d1                 �s�ԍ��f�o�b�O�����o�͂��܂�
        -d<name>[=text]     �e�L�X�g�}�N�����`���܂�
        -0                  8086 ����
        -1                  80186 ����
        -2                  80286 ����
        -3                  80386 ����
        -4                  80486 ����
        -m{t,s,m,c,l,h,f}   �������E���f��
                            (Tiny, Small, Medium, Compact, Large, Huge, Flat)
        -o                  allow C form of octal constants
        -3r                 386 ���W�X�^�Ăяo���K��
        -3s                 386 �X�^�b�N�Ăяo���K��
        -4r                 486 ���W�X�^�Ăяo���K��
        -4s                 486 �X�^�b�N�Ăяo���K��
        -zq or -q           ���b�Z�[�W���̏o�͂����܂���
        -?                  ���̃��b�Z�[�W��\�����܂�
:endsegment
:segment IS_RC
.
:endsegment
