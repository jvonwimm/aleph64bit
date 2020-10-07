        PARAMETER( LISDIM = 2500 )
        IMPLICIT INTEGER (A-Z)
        COMMON/TLIBUF/NTLMAX,LISFRE,IELEME
#if defined(DOC)
C
C!  These are the basic common and IMPLICIT definitions
C   used by the list package (TLIPAK) contained in the TPC
C   reconstruction program. This common is only used internally
C   by TLIPAK.  It is initialized by a call to TLINIT in TINIJO.
C   The list package itself is referenced by TPADPR and TISFIL.
C--------------------------------------------------------------------
#endif
