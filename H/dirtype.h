/* DRT_ values ( directive types ) */
/* group CONDDIR - INCLUDE must be first, INCLUDE the last of them. */
res( CONDDIR,   CondAsmDirective ) /* pp conditional assembly directive (IF, ELSE, ...) */
res( LOOPDIR,   LoopDirective )    /* pp loop directive (FOR, REPEAT, WHILE, ...) */
res( PURGE,     PurgeDirective )   /* pp PURGE directive */
res( INCLUDE,   IncludeDirective ) /* pp INCLUDE directive */
res( MACRO,     NULL ) /* pp MACRO directive */
res( CATSTR,    NULL ) /* pp TEXTEQU, CATSTR directives */
res( SUBSTR,    NULL ) /* pp SUBSTR directive */
res( MACINT,    NULL ) /* "macro" directives EXITM, ENDM, GOTO */
res( DATADIR,   NULL ) /* data definition directives DB, DW, DD, ... */
res( END,       EndDirective )
res( ERRDIR,    ErrorDirective ) /* v2.05: no longer preprocessor directives */
res( CPU,       cpu_directive )
res( LISTING,   ListingDirective )
res( SEGORDER,  SegOrderDirective )
res( SIMSEG,    SimplifiedSegDir )
res( HLLSTART,  HllStartDef )
res( HLLEXIT,   HllExitDef )
res( HLLEND,    HllEndDef )
res( STARTEXIT, StartupExitDirective )
res( MODEL,     ModelDirective )
res( RADIX,     RadixDirective )
#if COFF_SUPPORT
res( SAFESEH,   SafeSEHDirective )
#endif
res( INSTR,     InStrDef )
res( SIZESTR,   SizeStrDef )
#if AMD64_SUPPORT
res( EXCFRAME,  ExcFrameDirective )
#endif
res( STRUCT,    StructDirective )
res( TYPEDEF,   TypedefDirective )
res( RECORD,    RecordDirective )
res( COMM,      CommDirective )
res( EXTERN,    ExternDirective )
res( EXTERNDEF, ExterndefDirective )
res( PROTO,     ProtoDirective )
res( PUBLIC,    PublicDirective )
res( PROC,      ProcDef )
res( ENDP,      EndpDef )
res( LOCAL,     LocalDef )
res( INVOKE,    InvokeDirective )
res( ORG,       OrgDirective )
res( ALIGN,     AlignDirective )
res( SEGMENT,   SegmentDir )
res( ENDS,      EndsDir )
res( GROUP,     GrpDef )
res( ASSUME,    AssumeDirective )
res( LABEL,     LabelDirective )
res( ALIAS,     AliasDirective )
res( ECHO,      EchoDirective )
res( EQU,       EquDirective )
res( EQUALSGN,  EquDirective ) /* '=' directive */
#if INCLUDEBIN
res( INCBIN,    IncBinDirective )
#endif
res( INCLIB,    IncludeLibDirective )
res( NAME,      NameDirective )
res( OPTION,    OptionDirective )
res( CONTEXT,   ContextDirective )
