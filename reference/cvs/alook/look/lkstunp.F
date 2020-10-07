      SUBROUTINE LKSTUNP (ULNEW)
C -----------------------------------------------------------
C! set unpack list
C  the first call defines the default list ULDEF
CKEY LOOK UNPACK LIST
C - F.Ranjard - 911114
C - Input  : ULNEW / A : unpack list
C - ENTRY LKRSUNP
C   reset ULIST to ULDEF
C -----------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) ULNEW
      CHARACTER*80 ULDEF
#include "lkunpk.h"
      DATA IFI /0/
C -----------------------------------------------------------
C - 1st entry
      IF (IFI.EQ.0) THEN
         IFI = 1
         ULDEF = ULNEW
      ENDIF
C
C - next entry
      ULIST = ULNEW
      RETURN
C -----------------------------------------------------------
C
C - entry
      ENTRY LKRSUNP
      ULIST = ULDEF
      RETURN
C -----------------------------------------------------------
      END
#endif
