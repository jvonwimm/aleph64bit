      SUBROUTINE E4R2R3(ROWJ,COLI,JROW3,JROW4,ICOL1,ICOL2,ICOL3
     &,ICOL4)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR GIVE IJK FOR STOREYS IN R2 R3 OVERLAPING REGION OF EN
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "e4par0.h"
C RD RATIO NUMBER OF PADS IN A ROW REGION 3/NBER IN A ROW REGION 2
      PARAMETER(RD=3./2.)
C  R2-R3
      IF( (COLI.GT. COR311.AND. COLI.LT.COR312)
     &.OR.(COLI.GE. COR322.AND. COLI.LT.COR323))THEN
      RJ01=1.-1./(2.*RD)
      JROW3=INT((ROWJ-RJ01)*RD)
            JROW4=JROW3+1
            IF (COLI.GT. COR311.AND. COLI.LT.COR312)THEN
               ICOL3=ICOL1
               ICOL4=ICOL2
               ENDIF
            IF (COLI.GE.COR322.AND. COLI.LT.COR323)THEN
               ICOL3=ICOL2
               ICOL4=ICOL1
               ENDIF
         ENDIF
C R3-R2
      IF( (COLI.GT.COR312.AND. COLI.LT.COR313)
     &.OR.(COLI.GE.COR321.AND. COLI.LT.COR322))THEN
      RJ02=1.-RD/2.
      JROW3=INT((ROWJ-RJ02)/RD)
            JROW4=JROW3+1
            IF (COLI.GT. COR312.AND. COLI.LT.COR313)THEN
               ICOL3=ICOL2
               ICOL4=ICOL1
               ENDIF
            IF (COLI.GE.COR321 .AND. COLI.LT.COR322)THEN
               ICOL3=ICOL1
               ICOL4=ICOL2
               ENDIF
         ENDIF
CXX PHI REDEF SI NECESSAIRE
      IF(ICOL4.EQ.ICR311.OR.ICOL4.EQ.ICR321)THEN
         IF(JROW3.LT.JECMIN)JROW3=JR3MAX
         IF(JROW4.GT.JR3MAX)JROW4=JECMIN
         ENDIF
      IF(ICOL4.EQ.(ICR311-1).OR.ICOL4.EQ.(ICR321+1))THEN
         IF(JROW3.LT.JECMIN)JROW3=JR2MAX
         IF(JROW4.GT.JR2MAX)JROW4=JECMIN
         ENDIF
      RETURN
      END
#endif
