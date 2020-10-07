      SUBROUTINE MINFVB(KDTRA)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bit pattern in DTRA.
C
C     Author: Stephen Haywood      10-Feb-93
C
C     Input  : PYER,PYFR,YV0V banks
C              KDTRA  = index of DTRA bank
C     Output : vertex bits of DTRA bank
C
C     Called by MINTRA
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "pyfrjj.h"
#include "yv0vjj.h"
#include "dtrajj.h"
C
      DIMENSION INDV0(100),CHIV0(100)
C
#include "bmacro.h"
C
      NDTRA = LROWS(KDTRA)
C
C++   Find the number of main vertices.
C
      KPYER = NLINK('PYER',0)
      IF (KPYER.GT.0) THEN
         NPYER = LROWS(KPYER)
      ELSE
         NPYER = 0
      ENDIF
C
C++   Fill vertex bits corresponding to PYER vertices.
C
      KPYFR = NLINK('PYFR',0)
      IF (KPYFR.GT.0) THEN
         NPYFR = LROWS(KPYFR)
      ELSE
         NPYFR = 0
      ENDIF
C
      DO 100 I=1,NPYFR
         IVER = ITABL(KPYFR,I,JPYFVN)
         IF (IVER.GT.30) GOTO 100
         IDTRA = ITABL(KPYFR,I,JPYFTN)
         IF (IDTRA.GT.NDTRA) GOTO 100
         IND = KROW(KDTRA,IDTRA) + JDTRVB
         CALL SBIT1(IW(IND),IVER)
  100 CONTINUE
C
C++   Next, YV0V vertices.
C
      KYV0V = IW(NAMIND('YV0V'))
      IF (KYV0V.GT.0) THEN
         NYV0V = LROWS(KYV0V)
      ELSE
         NYV0V = 0
      ENDIF
C
C++   Check that there are not too many V0's. If the total number of
C++   vertices will excede 30, then the V0's are sorted in increasing
C++   chi-squared.
C
      IF (NPYER+NYV0V.LE.30) THEN
         DO 150 I=1,NYV0V
  150    INDV0(I) = I
      ELSE
         DO 160 I=1,NYV0V
  160    CHIV0(I) = RTABL(KYV0V,I,JYV0C2)
         CALL SORTZV(CHIV0,INDV0,NYV0V,+1,0,0)
      ENDIF
C
C++   Fill vertex bits corresponding to YV0V vertices.
C
      DO 200 I=1,NYV0V
         IYV0V = INDV0(I)
         IVER = NPYER + I
         IF (IVER.GT.30) GOTO 200
         DO 210 IDAU=0,1
            IDTRA = ITABL(KYV0V,IYV0V,JYV0K1+IDAU)
            IF (IDTRA.GT.NDTRA) GOTO 210
            IND = KROW(KDTRA,IDTRA) + JDTRVB
            CALL SBIT1(IW(IND),IVER)
  210    CONTINUE
  200 CONTINUE
C
      RETURN
      END
#endif
