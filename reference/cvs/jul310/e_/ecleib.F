      SUBROUTINE ECLEIB
C----------------------------------------------------------------------
C!  - CLOSE EIDT BANK
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C?
C!======================================================================
#ifndef DOC
#include "ecnamc.h"
#include "bcs.h"
#include "bmacro.h"
C
C? EIDT BK
C
      KEIDT=IW(NAEIDT)
       IF ( KEIDT.NE.0 ) THEN
C NBER OF USEFUL  DATA IN EIDT BK
      IUSE=LMHLEN+IW(KEIDT+LMHCOL)*IW(KEIDT+LMHROW)
      CALL AUBOS('EIDT',0,IUSE,KEIDT,IGARB)
        ENDIF
C
      RETURN
      END
#endif
