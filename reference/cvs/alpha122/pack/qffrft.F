      SUBROUTINE QFFRFT
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! Deal with FRFT banks and FRxy data cards
C!called from QFILL  
C!                                                   J.Boucrot  23.02.98
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      INTEGER GTSTUP
      LOGICAL XFR10,XFR12
C-----------------------------------------------------------------------
C Drop FRFT, NR=2 when it exists in 1989/1990 data (should not be the default)
      CALL ABRUEV(IRUN,IEVT)
      IF (GTSTUP('VD',IRUN).LE.2) KFRFT = NDROP('FRFT',2)
C
C   DEAL with FR12 card
C
      XFR12=IW(NAMIND('FR12')).NE.0
      IF (XFR12) THEN
         IND=NSWAP('FRFT',0,'FRFT',3)
         IND=NSWAP('FRFT',2,'FRFT',4)
         IND=NSWAP('FRFT',12,'FRFT',1)
         GO TO 999
      ENDIF
C
C   DEAL with FR10 card
C
      XFR10=IW(NAMIND('FR10')).NE.0
      IF (XFR10) THEN
         IND=NSWAP('FRFT',0,'FRFT',3)
         IND=NSWAP('FRFT',2,'FRFT',4)
         IND=NSWAP('FRFT',10,'FRFT',1)
         GO TO 999
      ENDIF
C
C Standard case : no FR12 or FR10 or FRF0 card : 
C then swap FRFT 0 to FRFT 3 to have 
C NAFRFT pointing to FRFT , NR=2

      IF (.NOT.XFRF0) IND=NSWAP('FRFT',0,'FRFT',3)
C
C The only remaining case is the presence of a FRF0 card: 
C then NAFRFT points to FTFT , NR=0
C and nothing has to be changed
C
 999  RETURN          
C
      END
#endif
