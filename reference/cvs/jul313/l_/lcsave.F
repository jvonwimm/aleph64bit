      SUBROUTINE LCSAVE
C------------------------------------------------------------------
C!   Save PECO, PEPT LCAL info in LPSV and LPES in case of Repro.
C!
C!   Authors   :  M.N.Minard
C!   Input     :  PECO PEPT
C!   Output    :  LPSV LPES
C!   Called by :  ECERAS
C!
C! In Case Of Reprocessing :
C!      Copy LCAL info from banks PECO and PEPT into banks LPSV and LPES
C!      this banks will be copied into PECO and PEPT at the end of julia
C!      when POT banks are made.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "pecojj.h"
#include "peptjj.h"
      DATA NAPECO / 0 /
      DATA NAPEPT / 0 /
#include "bmacro.h"
      IF ( NAPECO.EQ.0) THEN
        NAPECO = NAMIND('PECO')
        NAPEPT = NAMIND('PEPT')
      ENDIF
      KPECO = IW(NAPECO)
      KPEPT = IW(NAPEPT)
      NPECO = 0
      IF (KPECO.NE.0) NPECO = LROWS(KPECO)
      NCAL = 0
      IFCAL = 0
      DO IPECO = 1,NPECO
        IF ( ITABL(KPECO,IPECO,JPECKD).GE.192) THEN
          IF (NCAL.EQ.0) IFCAL = IPECO
          NCAL = NCAL + 1
        ENDIF
      ENDDO
      CALL BLIST (IW,'T+','LPSVLPES')
      IF (NCAL.NE.0) THEN
        LENG = LMHLEN + LPECOA*NCAL
        CALL AUBOS('LPSV',0,LENG,KLPSV,IGARB)
        IF ( IGARB.EQ.2) GO TO 999
        IW(KLPSV+LMHROW) = NCAL
        IW(KLPSV+LMHCOL) = LPECOA
        LENG = LMHLEN + LPEPTA*NCAL
        CALL AUBOS('LPES',0,LENG,KLPES,IGARB1)
        IF ( IGARB1.EQ.2) GO TO 999
        IW(KLPES+LMHROW) = NCAL
        IW(KLPES+LMHCOL) = LPEPTA
        IF ( IGARB+IGARB1.NE.0) THEN
          KLPSV = IW(NAMIND('LPSV'))
          KPECO = IW(NAPECO)
          KPEPT = IW(NAPEPT)
        ENDIF
        DO ICAL = 1,NCAL
          JLPSV = KROW(KLPSV,ICAL)
          JLPES = KROW(KLPES,ICAL)
          JPEPT = KROW(KPEPT,IFCAL+ICAL-1)
          JPECO = KROW(KPECO,IFCAL+ICAL-1)
          CALL UCOPY(RW(JPEPT+1),RW(JLPES+1),LPEPTA)
          CALL UCOPY(RW(JPECO+1),RW(JLPSV+1),LPECOA)
        ENDDO
      ENDIF
      RETURN
  999 CALL RERROR ('LCSAVE ',1,'NOT ENOUGH SPACE')
      RETURN
      END
#endif
