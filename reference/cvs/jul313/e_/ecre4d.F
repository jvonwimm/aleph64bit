      SUBROUTINE ECRE4D(IER)
C----------------------------------------------------------------------
C!  - CREATE E4DE BANK
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "e4dejj.h"
#include "rlunit.h"
#include "bmacro.h"
C
C  WE RESERVE ONE ROW FOR EACH TRACK
C GET NBER  OF TRACKS
      IER=0
      NAFRFT=NAMIND('FRFT')
      KFRFT=IW(NAFRFT)
      IF(KFRFT.EQ.0)GO TO 998
      NTRKS=LROWS(KFRFT)
C CREATE BANK
      CALL AUBOS('E4DE',0,NTRKS*LE4DEA+LMHLEN,KE4DE,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','E4DE')
      IW(KE4DE+LMHCOL)=LE4DEA
      IW(KE4DE+LMHROW)=NTRKS
      DO 88 NTK=1,NTRKS
      IW(KROW(KE4DE,NTK)+JE4DIK)=1
      IW(KROW(KE4DE,NTK)+JE4DIK+1)=1
      IW(KROW(KE4DE,NTK)+JE4DIK+2)=1
 88   CONTINUE
      RETURN
  998 CONTINUE
      IER=1
      IND = NDROP('E4DE',0)
      RETURN
      END
#endif
