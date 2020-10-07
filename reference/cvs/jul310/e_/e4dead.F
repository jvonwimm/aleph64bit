      SUBROUTINE E4DEAD(JTRAK,ICOL1,ICOL2,JROW1,JROW2,KSTAK)
C----------------------------------------------------------------------
C!  - LOOK IF THERE IS DEAD STOREYS IN THE 2X2 STOREYS USED IN ELECTRON
C!
C!  AUTHOR : D. PALLIN
C!
C!  JTRAK : INDEX OF TRACK IN FRFT BANK
C!  ICOL, JROW : TETA AND PHI FOR SELECTED STOREYS
C!  KSTAK : STACK OF SELECTED STOREYS
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "e4com2.h"
#include "edstjj.h"
#include "bmacro.h"
C   DEAD STOREYS
      NAEDST=NAMIND('EDST')
      KEDST=IW(NAEDST)
      IF(KEDST.EQ.0)RETURN
      NEDST=LROWS(KEDST)
      IF(NEDST.EQ.0)RETURN
      DO 1 I=1,NEDST
      JCOL=IW(KROW(KEDST,I)+JEDSTJ)
      IF(ICOL1.EQ.JCOL.OR.ICOL2.EQ.JCOL)THEN
           IROW=IW(KROW(KEDST,I)+JEDSFI)
           IF(JROW1.EQ.IROW.OR.JROW2.EQ.IROW)THEN
              KST=IW(KROW(KEDST,I)+JEDSDK)
              IF(KSTAK.EQ.KST)IJDEAD(KSTAK)=IJDEAD(KSTAK)+1
           ENDIF
      ENDIF
 1    CONTINUE
      RETURN
      END
#endif
