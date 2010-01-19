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
* Description:  FP fixups for 16-bit code. These fixups allow the program
* loader to replace FP instructions by calls to an FP emulation library.
* Used by Win16 and OS/2 16bit.
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "directiv.h"
#include "extern.h"
#include "fixup.h"
#include "mangle.h"
#include "myassert.h"
#include "segment.h"
#include "omf.h"

typedef enum {
    FPP_WAIT,
    FPP_NORMAL,
    FPP_ES,
    FPP_CS,
    FPP_SS,
    FPP_DS,
    FPP_FS,
    FPP_GS
} fp_patches;

static const char * const FPPatchName[] = {
    "FIWRQQ",
    "FIDRQQ",
    "FIERQQ",
    "FICRQQ",
    "FISRQQ",
    "FIARQQ",
    "FIFRQQ",
    "FIGRQQ"
};

static const char * const FPPatchAltName[] = {
    NULL,
    NULL,
    NULL,
    "FJCRQQ",
    "FJSRQQ",
    "FJARQQ",
    "FJFRQQ",
    "FJGRQQ"
};

ret_code AddFloatingPointEmulationFixup( struct code_info *CodeInfo, bool secondary )
/************************************************************************************/
{
    fp_patches patch;
    struct asm_sym *sym;
    const char * const *patch_name_array;
    struct asmfixup *fixup;
    int_32 data;

    patch_name_array = ( secondary ? FPPatchAltName : FPPatchName );

    if( CodeInfo->token == T_FWAIT ) {
        patch = FPP_WAIT;
    } else {
        switch( CodeInfo->prefix.RegOverride ) {
        case EMPTY:      patch = FPP_NORMAL;        break;
        case ASSUME_ES:  patch = FPP_ES;            break;
        case ASSUME_CS:  patch = FPP_CS;            break;
        case ASSUME_SS:  patch = FPP_SS;            break;
        case ASSUME_DS:  patch = FPP_DS;            break;
        case ASSUME_FS:  patch = FPP_FS;            break;
        case ASSUME_GS:  patch = FPP_GS;            break;
        default:
            never_reach();
        }
    }

    if( patch_name_array[patch] == NULL )
        return( NOT_ERROR ); /* no fixup needed */

    DebugMsg(("AddFloatingPointEmulationFixup( %X, %u )\n", CodeInfo, secondary ));
    /* put out an extern def for the patch if not done already */
    sym = SymSearch( patch_name_array[patch] );
    if( sym == NULL ) {
        sym = MakeExtern( patch_name_array[patch], MT_FAR, NULL, NULL, USE16 );
        SetMangler( sym, NULL, LANG_NONE );
    }
    /* no need for fixups if no object file is written */
    if ( write_to_file == FALSE )
        return( NOT_ERROR );

    /* make sure the next 2 bytes in code stream aren't separated */
    if( Options.output_format == OFORMAT_OMF &&
       (CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc + 2) >= MAX_LEDATA )
        omf_FlushCurrSeg();

    fixup = AddFixup( sym, FIX_OFF16, OPTJ_NONE );
    /* v2.02: use TARGET, as Masm does! */
    //fixup->frame = FRAME_LOC;
    fixup->frame = FRAME_TARG;
    /* v2.02: this has been done in AddFixup() already */
    //fixup->fixup_loc = GetCurrOffset();
    data = 0;
    return ( store_fixup( fixup, &data ) );
}
