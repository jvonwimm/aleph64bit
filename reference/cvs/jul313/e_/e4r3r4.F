      SUBROUTINE E4R3R4(ROWJ,COLI,JROW3,JROW4,ICOL1,ICOL2,ICOL3
     &,ICOL4)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR GIVE IJK FOR STOREYS IN R3 R4 OVERLAPING REGION OF EN
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "e4par0.h"
C RD RATIO NUMBER OF PADS IN A ROW REGION 4/NBER IN A ROW REGION 3
      PARAMETER(RD=4./3.)
C  R3-R4
      IF( (COLI.GT.COR411.AND. COLI.LT.COR412)
     &.OR.(COLI.GE.COR422.AND. COLI.LT.COR423))THEN
      RJ01=1.-1./(2.*RD)
      JROW3=INT((ROWJ-RJ01)*RD)
C
            JROW4=JROW3+1
            IF (COLI.GT.COR411.AND. COLI.LT.COR412)THEN
               ICOL3=ICOL1
               ICOL4=ICOL2
               ENDIF
            IF (COLI.GE.COR422.AND. COLI.LT.COR423)THEN
               ICOL3=ICOL2
               ICOL4=ICOL1
               ENDIF
         ENDIF
C R4-R3
      IF( (COLI.GT.COR412.AND. COLI.LT.COR413)
     &.OR.(COLI.GE.COR421.AND. COLI.LT.COR422))THEN
      RJ02=1.-RD/2.
      JROW3=INT((ROWJ-RJ02)/RD)
            JROW4=JROW3+1
            IF (COLI.GT.COR412.AND. COLI.LT.COR413)THEN
               ICOL3=ICOL2
               ICOL4=ICOL1
               ENDIF
            IF (COLI.GE.COR421.AND. COLI.LT.COR422)THEN
               ICOL3=ICOL1
               ICOL4=ICOL2
               ENDIF
         ENDIF
CXX PHI REDEF SI NECESSAIRE
      IF(ICOL4.EQ.ICR411.OR.ICOL4.EQ.ICR421)THEN
         IF(JROW3.LT.JECMIN)JROW3=JR4MAX
         IF(JROW4.GT.JR4MAX)JROW4=JECMIN
         ENDIF
      IF(ICOL4.EQ.(ICR411-1).OR.ICOL4.EQ.(ICR421+1))THEN
         IF(JROW3.LT.JECMIN)JROW3=JR3MAX
         IF(JROW4.GT.JR3MAX)JROW4=JECMIN
         ENDIF
      RETURN
      END
#endif
